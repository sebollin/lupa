# Datos operativos sintéticos y neutrales

Conjunto enteramente sintético, sin vocabulario ni convenciones de un
país, para demostrar el recorrido general de `lupa`. Siembra los mismos
tipos de problemas que el ejemplo administrativo: formatos heterogéneos,
fechas mezcladas, ausentes disfrazados, espacios y mayúsculas
inconsistentes, valores extremos, una columna constante, una columna
duplicada, una fila duplicada y un identificador repetido con datos
contradictorios. No representa personas, operaciones ni registros
reales.

## Uso

``` r
datos_operativos
```

## Formato

Un data frame con 13 filas y 10 variables:

  - id\_registro:
    
    Identificador interno, con una repetición contradictoria.

  - codigo\_usuario:
    
    Código sintético con formatos heterogéneos y un ausente disfrazado.

  - fecha\_evento:
    
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

  - id\_copia:
    
    Copia redundante del identificador interno.

  - id\_evento:
    
    Identificador sintético de alta unicidad.

## Fuente

Generación sintética incluida en `data-raw/datos_operativos.R`.

## Ver también

[datos\_administrativos](https://sebollin.github.io/lupa/reference/datos_administrativos.md),
`analizar()`, `perfilar()`

## Ejemplos

``` r
data(datos_operativos)
analisis <- analizar(datos_operativos, analizar_dependencias = FALSE)
analisis$perfil$hallazgos[, c("columna", "tipo_hallazgo", "severidad")]
#>           columna             tipo_hallazgo  severidad
#> 1  codigo_usuario         alta_cardinalidad sospechoso
#> 2  codigo_usuario     faltantes_disfrazados      error
#> 3    fecha_evento         alta_cardinalidad sospechoso
#> 4    fecha_evento     faltantes_disfrazados      error
#> 5    fecha_evento     formatos_fecha_mixtos      error
#> 6    fecha_evento   tipo_declarado_distinto sospechoso
#> 7           canal         alta_cardinalidad sospechoso
#> 8           canal                 faltantes sospechoso
#> 9           canal     faltantes_disfrazados      error
#> 10          canal        espacios_sobrantes sospechoso
#> 11          canal mayusculas_inconsistentes sospechoso
#> 12          monto     faltantes_disfrazados sospechoso
#> 13          monto                  outliers sospechoso
#> 14        sistema                 constante sospechoso
#> 15       contacto         alta_cardinalidad sospechoso
#> 16      id_evento     posible_identificador         ok
#> 17           <NA>          filas_duplicadas      error
#> 18    id_registro       columnas_duplicadas sospechoso
#> 19       contacto     dato_personal_posible         ok
```
