# Guardar y recuperar un histórico de calidad

Persiste el data frame versionado mediante RDS de base R. La escritura
no reemplaza un archivo existente salvo consentimiento explícito.

## Usage

``` r
guardar_historico(historico, archivo, sobrescribir = FALSE)

leer_historico(archivo)
```

## Arguments

- historico:

  Objeto creado por
  [`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md).

- archivo:

  Ruta del archivo RDS. Una ruta que ya es un directorio se rechaza: el
  archivo va dentro, con su nombre.

- sobrescribir:

  Si se permite reemplazar un archivo existente.

## Value

`guardar_historico()` devuelve invisiblemente la ruta normalizada;
`leer_historico()` devuelve un `historico_calidad` validado.

## See also

`guardar_historico()`,
[`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md)

[`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md),
[`comparar_evaluaciones()`](https://sebollin.github.io/lupa/reference/comparar_evaluaciones.md)

## Examples

``` r
archivo <- tempfile(fileext = ".rds")
guardar_historico(historico_calidad(), archivo)
leer_historico(archivo)
#>  [1] version_esquema               nivel                        
#>  [3] id_registro                   id_medida                    
#>  [5] id_medicion                   fecha                        
#>  [7] perfil                        regla                        
#>  [9] metrica                       metrica_especifica           
#> [11] metrica_instanciada           dimension                    
#> [13] factor                        granularidad                 
#> [15] tipo_resultado                entidad                      
#> [17] atributo                      fila                         
#> [19] objeto_medible                n_elementos                  
#> [21] resultado                     agregacion                   
#> [23] identidad_tabla               configuracion_modelo         
#> [25] configuracion_marco           configuracion_tipos_resultado
#> [27] configuracion_aplicabilidad   configuracion_perfil         
#> <0 rows> (or 0-length row.names)
unlink(archivo)
```
