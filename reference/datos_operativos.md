# Datos operativos sintéticos y neutrales

Conjunto enteramente sintético, sin vocabulario ni convenciones de un
país, para demostrar el recorrido general de `lupa`. Siembra los mismos
tipos de problemas que el ejemplo administrativo: formatos heterogéneos,
fechas mezcladas, ausentes disfrazados, espacios y mayúsculas
inconsistentes, valores extremos, una columna constante, una columna
duplicada, una fila duplicada y un identificador repetido con datos
contradictorios. No representa personas, operaciones ni registros
reales.

## Usage

``` r
datos_operativos
```

## Format

Un data frame con 13 filas y 10 variables:

- id_registro:

  Identificador interno, con una repetición contradictoria.

- codigo_usuario:

  Código sintético con formatos heterogéneos y un ausente disfrazado.

- fecha_evento:

  Fechas en varios formatos y un ausente disfrazado.

- canal:

  Categoría con variantes de capitalización, espacios y ausentes
  disfrazados.

- monto:

  Importes con sentinelas, cero, un negativo y un valor extremo.

- zona:

  Categoría operativa ficticia.

- sistema:

  Columna constante.

- contacto:

  Direcciones sintéticas y un patrón anómalo.

- id_copia:

  Copia redundante del identificador interno.

- id_evento:

  Identificador sintético de alta unicidad.

## Source

Generación sintética incluida en `data-raw/datos_operativos.R`.

## See also

[datos_administrativos](https://sebollin.github.io/lupa/reference/datos_administrativos.md),
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)

## Examples

``` r
data(datos_operativos)
analisis <- analizar(datos_operativos, analizar_dependencias = FALSE)
analisis$perfil$hallazgos[, c("columna", "tipo_hallazgo", "severidad")]
#>           columna               tipo_hallazgo  severidad
#> 1  codigo_usuario           alta_cardinalidad sospechoso
#> 2  codigo_usuario       faltantes_disfrazados      error
#> 3    fecha_evento                  casi_clave sospechoso
#> 4    fecha_evento           alta_cardinalidad sospechoso
#> 5    fecha_evento       faltantes_disfrazados      error
#> 6    fecha_evento       formatos_fecha_mixtos      error
#> 7    fecha_evento     tipo_declarado_distinto sospechoso
#> 8           canal           alta_cardinalidad sospechoso
#> 9           canal                   faltantes sospechoso
#> 10          canal       faltantes_disfrazados      error
#> 11          canal          espacios_sobrantes sospechoso
#> 12          canal   mayusculas_inconsistentes sospechoso
#> 13          monto                  casi_clave sospechoso
#> 14          monto       faltantes_disfrazados sospechoso
#> 15          monto                    outliers sospechoso
#> 16        sistema                   constante sospechoso
#> 17       contacto                  casi_clave sospechoso
#> 18       contacto           alta_cardinalidad sospechoso
#> 19      id_evento                  casi_clave sospechoso
#> 20      id_evento       posible_identificador         ok
#> 21 codigo_usuario casi_duplicados_vocabulario         ok
#> 22   fecha_evento casi_duplicados_vocabulario         ok
#> 23          canal casi_duplicados_vocabulario sospechoso
#> 24           zona casi_duplicados_vocabulario sospechoso
#> 25           <NA>            filas_duplicadas      error
#> 26    id_registro         columnas_duplicadas sospechoso
#> 27       contacto       dato_personal_posible         ok
```
