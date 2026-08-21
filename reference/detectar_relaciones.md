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
  tope_memoria_mb = Inf
)
```

## Arguments

- tabla1, tabla2:

  Objetos que heredan de `data.frame`.

- muestra:

  Máximo de filas del lado verificado que se usan para estimar cada
  cobertura. El muestreo es sistemático y reproducible; el lado de
  referencia no se muestrea. Use `Inf` para calcular todo sin muestreo.

- columnas_candidatas:

  Lista de dos vectores de nombres, para `tabla1` y `tabla2`, que
  declara las columnas que pueden participar. `NULL` conserva la
  exploración completa por compatibilidad.

- umbral_cobertura:

  Umbral usado por la poda de cardinalidades imposibles.

- podar:

  Si se aplican las podas que cambiarían lo informado —tipos
  incompatibles y cardinalidades imposibles—. `FALSE` por omisión: sólo
  se aplica la poda cierta, que no cambia ninguna fila.

- tope_memoria_mb:

  Presupuesto de memoria para las filas comparadas, en megabytes. Las
  combinaciones pendientes se declaran como podas cuando se alcanza;
  `Inf` no limita el procesamiento.

## Value

Data frame con columnas comparadas, cardinalidad, coincidencias,
coberturas de integridad referencial en ambas direcciones y
`motivo_poda`, que sólo tiene valor en los pares no comparados. Los
atributos `filas_totales`, `filas_analizadas` y `muestreado` documentan
el muestreo; `podas`, `n_pares_totales`, `n_pares_comparados` y
`n_pares_podados` documentan qué se comparó y qué no.

## Details

`columnas_candidatas` permite evitar la exploración de columnas que el
usuario sabe que no pueden participar. El costo crece con el producto de
anchos: dos tablas de treinta columnas son novecientas combinaciones por
par de tablas, y declarar cuáles pueden participar es lo que lo vuelve
manejable.

**Hay dos clases de poda y el paquete no las trata igual.** Dos columnas
de la misma familia con rangos numéricos disjuntos no comparten ningún
valor, y eso se sabe sin comparar: la fila sale como siempre
—`sin_coincidencias`, con cobertura cero— y la comparación se ahorra.
Esa poda está siempre activa porque no cambia lo que el objeto informa.

Las otras dos sí lo cambiarían. Familias distintas parece decisivo y no
lo es: una columna de texto puede guardar `"2020-01-05"` y coincidir con
una de fecha. Y una cardinalidad imposible no dice que no haya
coincidencias, dice que no alcanzan `umbral_cobertura`, que es otra
cosa. Por eso van detrás de `podar = TRUE`, y cuando se aplican **el par
no desaparece**: sale con `cardinalidad = "sin_comparar"`, coberturas
`NA` y su motivo en `motivo_poda`. Un par que no se evaluó no es un par
sin relación.

Todas las podas, de las dos clases, quedan además en el atributo `podas`
con su motivo y su detalle.

Cuando una tabla supera `muestra`, la función estima cada cobertura con
una muestra sistemática del lado que se verifica y conserva completo el
conjunto de referencia. La cardinalidad y la cantidad de valores comunes
siempre se calculan con ambas columnas completas.

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
```
