# Comparar dos perfiles y detectar deriva estructural

Compara entregas sin exigir que tengan las mismas columnas. Devuelve
cambios de esquema, tipos, faltantes, cardinalidad, rango, patrones y
hallazgos como un objeto de datos filtrable.

## Usage

``` r
comparar_perfiles(anterior, actual, umbral_cambio = 0.05, umbral_error = 0.2)
```

## Arguments

- anterior, actual:

  Objetos producidos por
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

- umbral_cambio:

  Diferencia mínima para considerar significativo un cambio de
  proporción, cardinalidad o rango relativo. Cinco puntos porcentuales
  evita elevar variaciones pequeñas a hallazgo.

- umbral_error:

  Diferencia a partir de la cual un aumento de faltantes o un patrón
  nuevo se clasifica como `error`.

## Value

Data frame `deriva_perfil`. `severidad` usa el factor ordenado
`ok < sospechoso < error`; los cambios menores permanecen como filas
`ok` para que la serie sea exportable sin ocultar diferencias.

## Details

Los patrones se comparan sobre el resumen acotado que conserva cada
perfil, no sobre los valores originales ni una distribución completa. Si
las dos corridas usaron configuraciones de patrones diferentes, se
informa un error de comparabilidad y esa parte de la comparación se
omite.

Los nombres de columnas y las configuraciones que contienen texto del
usuario se comparan por sus bytes, no por la marca de codificación ni
por la intercalación del locale.

Eso alcanza mientras los bytes lleguen intactos, y hay un caso en que no
llegan y **no depende de este paquete**: el formato RDS 3 —el de
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) por omisión— anota la
codificación nativa de quien escribe, y al leer *traduce* desde ella el
texto que no declara la suya. Un perfil guardado bajo `LC_CTYPE=C` y
releído bajo otro locale puede volver con el texto cambiado; en Windows
el cambio es silencioso, porque ahí la conversión no falla.

Cuando eso pasa, el perfil releído **está** alterado, y conviene saber
qué alcanza a ver esta función. Compara las **propiedades medidas** de
cada columna y los hallazgos, no los valores uno por uno: la columna
`aspecto` del resultado nombra, fila por fila, qué se comparó. **Un
cambio de texto que no altera ninguna propiedad medida no aparece**: por
ejemplo, si la moda de una columna pasa de `"Basico"` a `"Casico"`
—mismo largo, misma forma de patrón, mismos conteos—, la comparación
devuelve cero filas. Por eso el remedio ante una relectura sospechosa no
es confiar en la comparación para detectarla.

El remedio es una línea, `saveRDS(perfil, archivo, version = 2)`, porque
ese formato no anota codificación nativa y por lo tanto no traduce nada.
La persistencia propia del paquete
—[`guardar_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md)
y
[`guardar_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md)—
ya lo usa. Las claves internas que agrupan hallazgos también se
normalizan por bytes; por eso dos perfiles idénticos no emiten avisos
espurios bajo C.

Las columnas que aparecen o desaparecen generan cambios estructurales de
severidad `error`, pero no impiden comparar las columnas compartidas. Un
hallazgo de una columna retirada no se presenta como resuelto.

Un hallazgo que ya no aparece se informa como `resuelto` sólo si el
diagnóstico volvió a evaluarse. Si el perfil nuevo lo declinó —y lo dice
en su `cobertura_diagnosticos`—, el cambio se informa como `no_evaluado`
con severidad `sospechoso`, porque no se sabe si el hallazgo sigue:
dejar de mirar no es lo mismo que arreglar.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md),
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md),
[`comparar_equivalencia()`](https://sebollin.github.io/lupa/reference/comparar_equivalencia.md)

## Examples

``` r
anterior <- perfilar(data.frame(codigo = c("AA1", "AA2")),
                     fecha = as.POSIXct("2026-01-01", tz = "UTC"))
actual <- perfilar(data.frame(codigo = c("AA1", "B-2"), nueva = 1:2),
                   fecha = as.POSIXct("2026-02-01", tz = "UTC"))
comparar_perfiles(anterior, actual)
#>   columna aspecto    cambio severidad valor_anterior valor_actual delta
#> 1   nueva columna aparecida     error           <NA>        nueva    NA
#> 2  codigo  patron aparecido     error           <NA>          A-9   0.5
#>   cambio_relativo significativo fecha_anterior fecha_actual
#> 1              NA          TRUE     2026-01-01   2026-02-01
#> 2              NA          TRUE     2026-01-01   2026-02-01
#>                                                  descripcion
#> 1 Apareció una columna que no estaba en la entrega anterior.
#> 2                       Apareció un patrón de formato nuevo.
#>                                        evidencia
#> 1 No existe una base anterior para sus métricas.
#> 2                      Proporción actual: 0.5000
```
