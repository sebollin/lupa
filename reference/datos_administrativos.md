# Datos administrativos sintéticos con problemas sembrados

Conjunto enteramente sintético para demostrar el motor de profiling.
Incluye cédulas con formatos heterogéneos, fechas mezcladas, faltantes
disfrazados, una columna constante, valores extremos, una fila duplicada
y registros con el mismo identificador pero contenidos distintos. No
representa personas ni registros de ningún organismo.

## Uso

``` r
datos_administrativos
```

## Formato

Un data frame con 13 filas y 10 variables:

  - id\_persona:
    
    Identificador interno, con una repetición contradictoria.

  - cedula:
    
    Documento sintético con formatos correctos e incorrectos.

  - fecha\_nacimiento:
    
    Fechas en varios formatos y un faltante disfrazado.

  - sexo:
    
    Categoría sintética con un faltante disfrazado.

  - ingreso:
    
    Importes con sentinelas numéricos y un valor extremo.

  - departamento:
    
    Categoría administrativa.

  - pais:
    
    Columna constante.

  - correo:
    
    Direcciones sintéticas y un patrón anómalo.

  - id\_copia:
    
    Copia redundante del identificador interno.

  - id\_tramite:
    
    Identificador sintético de alta unicidad.

## Fuente

Generación sintética incluida con el paquete.

## Ver también

`perfilar()`, `planificar_limpieza()`

## Ejemplos

``` r
data(datos_administrativos)
perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
perfil$columnas[, c("columna", "tipo_inferido", "prop_faltantes_totales")]
#>             columna tipo_inferido prop_faltantes_totales
#> 1        id_persona         doble             0.00000000
#> 2            cedula         texto             0.07692308
#> 3  fecha_nacimiento         fecha             0.07692308
#> 4              sexo         texto             0.15384615
#> 5           ingreso         doble             0.07692308
#> 6      departamento         texto             0.00000000
#> 7              pais         texto             0.00000000
#> 8            correo         texto             0.00000000
#> 9          id_copia         doble             0.00000000
#> 10       id_tramite identificador             0.00000000
```
