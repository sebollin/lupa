# Detectar relaciones entre dos tablas

Examina los pares de columnas declarados y describe su cardinalidad a
partir de la unicidad completa de cada lado. La cobertura
`tabla1_en_tabla2` es la proporción de valores no ausentes de la primera
columna que existe en la segunda; la cobertura inversa se informa de
forma simétrica. Así se puede escoger la dirección PK/FK sin imponerla
de antemano.

## Usage

``` r
detectar_relaciones(
  tabla1,
  tabla2,
  muestra = 1e+05,
  columnas_candidatas = NULL,
  umbral_cobertura = 0.9,
  podar = FALSE,
  tope_memoria_mb = Inf,
  max_filas_huerfanas = 1000L,
  .rangos = NULL,
  .rellenos_finales = NULL,
  .trazar_huerfanas = TRUE
)
```

## Arguments

- tabla1, tabla2:

  Objetos que heredan de `data.frame`.

- muestra:

  Se conserva por compatibilidad y se valida como antes, pero ya no
  limita nada: las coberturas se calculan sobre todas las filas.

- columnas_candidatas:

  Lista de dos vectores de nombres, para `tabla1` y `tabla2`, que
  declara las columnas que pueden participar. `NULL` conserva la
  exploración completa por compatibilidad.

- umbral_cobertura:

  Umbral usado por la poda de cardinalidades imposibles, y cobertura
  mínima de un lado para ubicar sus filas huérfanas: por debajo se
  cuentan pero no se nombran.

- podar:

  Si se aplican las podas que cambiarían lo informado —tipos
  incompatibles y cardinalidades imposibles—. `FALSE` por omisión: sólo
  se aplica la poda cierta, que no cambia ninguna fila.

- tope_memoria_mb:

  Presupuesto de memoria para las filas comparadas, en megabytes. Las
  combinaciones pendientes se declaran como podas cuando se alcanza;
  `Inf` no limita el procesamiento. Los conteos y las trazas de filas
  huérfanas son parte de cada fila comparada y cuentan en el
  presupuesto.

- max_filas_huerfanas:

  Tope de índices de fila que conserva cada traza de filas huérfanas,
  como `max_filas_hallazgo` en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
  Por defecto es `1000`; cuando se supera, el `estado` de la traza queda
  `truncada` y el `total` se conserva. `Inf` desactiva el tope.

- .rangos:

  Uso interno de
  [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md).
  Lista nombrada por `tabla1` y `tabla2`, con rangos y su origen para
  cada columna. La poda por rangos solo acepta rangos de la columna
  completa o del universo DBI. Una columna cuya lectura no permite
  comparar exacto lo declara en `no_comparable`, y sus pares salen
  `sin_comparar` con ese motivo.

- .rellenos_finales:

  Uso interno de
  [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md).
  Declara las columnas cuyo controlador compara ignorando espacios
  finales. Se aplica solo al par que tiene una columna declarada, para
  conservar la semantica de comparacion del motor sin convertir todas
  las columnas de texto.

- .trazar_huerfanas:

  Uso interno. `FALSE` omite las columnas `huerfanas_tabla1` y
  `huerfanas_tabla2`, y la pasada por columna que cuestan; los cuatro
  conteos se calculan siempre.

## Value

Data frame con `columna_tabla1`, `columna_tabla2`, `cardinalidad`
—`1:1`, `1:m`, `m:1`, `m:m`, `sin_coincidencias` o `sin_comparar`—,
`n_valores_comunes`, coberturas de integridad referencial en ambas
direcciones y `motivo_poda`, que sólo tiene valor en los pares no
comparados. Después, las filas huérfanas de cada lado: los conteos
`n_filas_huerfanas_tabla1`, `n_filas_huerfanas_tabla2`,
`n_valores_huerfanos_tabla1` y `n_valores_huerfanos_tabla2`, y las
columnas-lista `huerfanas_tabla1` y `huerfanas_tabla2`, con una traza
por par (`resultado$huerfanas_tabla2[[i]]$indices_fila`). Los atributos
`filas_totales`, `filas_analizadas` y `muestreado` documentan el
alcance: `filas_analizadas` son todas las filas y `muestreado` vale
`FALSE`; `podas`, `n_pares_totales`, `n_pares_comparados` y
`n_pares_podados` documentan qué se comparó y qué no.

## Details

Los valores se comparan por su identidad. Los números, por su valor; los
instantes y las fechas también, con su fracción: dos instantes del mismo
segundo no coinciden, y un instante sin fracción se escribe al segundo
—`2020-01-01 10:00:00`— para que un texto con esa escritura lo
encuentre. Un texto con bytes que no son UTF-8 válido se compara por sus
bytes, como `%in%`: no es un ausente, y dos valores así distintos no
coinciden.

`columnas_candidatas` permite evitar la exploración de columnas que el
usuario sabe que no pueden participar. El costo crece con el producto de
anchos: dos tablas de treinta columnas son novecientas combinaciones por
par de tablas, y declarar cuáles pueden participar es lo que lo vuelve
manejable.

**Hay dos clases de poda y el paquete no las trata igual.** Dos columnas
de la misma familia con rangos numéricos disjuntos, medidos sobre la
columna completa o sobre el universo DBI, no comparten ningún valor, y
eso se sabe sin comparar: la fila sale como siempre
—`sin_coincidencias`, con cobertura cero— y la comparación se ahorra.
Esa poda está siempre activa porque no cambia lo que el objeto informa.

Las otras dos sí lo cambiarían. Familias distintas parece decisivo y no
lo es: una columna de texto puede guardar `"2020-01-05"` y coincidir con
una de fecha. Y una cardinalidad imposible no dice que no haya
coincidencias, dice que no alcanzan `umbral_cobertura`, que es otra
cosa. La cota es por filas, como la cobertura: con `k` valores distintos
en `tabla1`, las filas cubiertas de `tabla2` no superan las de sus `k`
valores más frecuentes. Por eso van detrás de `podar = TRUE`, y cuando
se aplican **el par no desaparece**: sale con
`cardinalidad = "sin_comparar"`, coberturas `NA` y su motivo en
`motivo_poda`. Un par que no se evaluó no es un par sin relación.

Dos pares no se comparan nunca, con `podar` o sin él, porque la
comparación no daría una respuesta sino una falsa. Si una tabla tiene
dos columnas con el mismo nombre, el nombre no dice cuál de las dos es:
sus pares salen `sin_comparar` con `motivo_poda = "nombre_repetido"`. Y
una fecha contra una fecha-hora no comparte escritura —`2020-01-01`
contra `2020-01-01 00:00:00`—, así que ningún valor coincidiría aunque
fueran los mismos días: el par sale con
`motivo_poda = "fecha_contra_instante"`. Convertir una de las dos a la
clase de la otra, con `tz` explícito, y volver a comparar. Tampoco entra
una columna que no tiene un valor comparable por fila: una compuesta
—matriz, tabla anidada, valores que no se escriben como texto, como una
`vctrs_rcrd`—, una lista de objetos —de vectores, de geometrías, de
`NULL`— o una magnitud con unidad (`units`), cuyo número sin la unidad
no es su valor. Sus pares salen `sin_comparar` con
`motivo_poda = "columna_no_comparable"`, y el detalle en `podas` dice
cuál de las tres es; la función sigue con las demás columnas.

Todas las podas, de las dos clases, quedan además en el atributo `podas`
con su motivo y su detalle.

La cardinalidad, la cantidad de valores comunes y las dos coberturas se
calculan con ambas columnas completas: la cobertura cuenta las filas de
cada valor distinto, así que sale exacta por el costo de una pasada más
por columna, sin una muestra que pueda alinearse con los datos.

**Las filas huérfanas.** Cada par comparado dice cuántas filas y cuántos
valores distintos de cada tabla no están en la otra:
`n_filas_huerfanas_tabla1`, `n_filas_huerfanas_tabla2`,
`n_valores_huerfanos_tabla1` y `n_valores_huerfanos_tabla2`. Es lo que
la cobertura deja afuera, contado sobre las mismas filas y con la misma
identidad: otra caja o un espacio al borde son otro valor. **Un ausente
no es huérfano**: no entra en el denominador de la cobertura y tampoco
en estos conteos, así que la cobertura de un lado es uno menos sus filas
huérfanas sobre sus filas no ausentes.

`huerfanas_tabla1` y `huerfanas_tabla2` dicen además **cuáles** son, con
la forma de la trazabilidad de un hallazgo de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
una lista por par con `indices_fila` —posiciones en la tabla que se
recibió, en su orden—, `total`, `mostrados`, `truncado`, `limite`,
`estado` y `alcance`. Ubicarlas cuesta una pasada más por la columna, y
en dos columnas sin relación casi todas las filas son huérfanas, así que
sólo se ubican en el lado casi cubierto: con cobertura de
`umbral_cobertura` a menos de 1. Por debajo del umbral la traza sale
`no_disponible` con `alcance = "cobertura_bajo_umbral"` y conserva el
`total`. Sin filas huérfanas no hay nada que ubicar: la traza sale vacía
—`no_disponible`, con `total` `NA`— junto a un conteo de cero. En un par
`sin_comparar` los cuatro conteos son `NA` y las trazas, `no_disponible`
con `alcance = "no_evaluado"`.

**Los valores huérfanos no se publican**, sólo sus filas. Un índice no
dice nada de la celda; el valor de una columna que se compara con otra
suele ser un identificador, y esta función no sabe si es un dato
personal. Con las filas el valor está a una línea, para quien tiene la
tabla: `tabla2$columna[indices]`.

Se compara una columna contra una columna. Una clave compuesta se
compara componente por componente, y una combinación que no existe pero
cuyos componentes existen por separado no sale huérfana: para verla hay
que pegar los componentes en una columna de cada tabla y comparar ésa.

## See also

[`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md),
[`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md),
[`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md)

## Examples

``` r
personas <- data.frame(id = 1:3)
tramites <- data.frame(persona_id = c(1, 1, 3, 4))
detectar_relaciones(personas, tramites)
#>   columna_tabla1 columna_tabla2 cardinalidad n_valores_comunes
#> 1             id     persona_id          1:m                 2
#>   cobertura_tabla1_en_tabla2 cobertura_tabla2_en_tabla1 motivo_poda
#> 1                  0.6666667                       0.75        <NA>
#>   n_filas_huerfanas_tabla1 n_filas_huerfanas_tabla2 n_valores_huerfanos_tabla1
#> 1                        1                        1                          1
#>   n_valores_huerfanos_tabla2 huerfanas_tabla1 huerfanas_tabla2
#> 1                          1     no_dispo....     no_dispo....

# Con casi todos los trámites cubiertos, la traza nombra el que no lo está.
tramites <- data.frame(persona_id = c(rep(1:3, 6), 4))
relacion <- detectar_relaciones(personas, tramites)
relacion$n_filas_huerfanas_tabla2
#> [1] 1
relacion$huerfanas_tabla2[[1]]$indices_fila
#> [1] 19
```
