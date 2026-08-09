# Proponer escalas y roles de las variables

Separa el tipo de almacenamiento, el tipo implícito y la escala de
medición. Una propuesta basada sólo en valores nunca queda confirmada:
`1, 2, 3` puede ser conteo, código o nivel. Las clases `ordered`,
`factor`, `logical`, las clases temporales y el metadato `measure` sí
constituyen declaraciones.

## Uso

``` r
clasificar_variables(
  datos,
  perfil = NULL,
  metadatos = NULL,
  max_niveles = 100L,
  muestra = 1e+05,
  proteger_datos_personales = TRUE
)
```

## Argumentos

  - datos:
    
    Tabla que se desea clasificar.

  - perfil:
    
    Perfil opcional de los mismos datos.

  - metadatos:
    
    Declaraciones opcionales por columna.

  - max\_niveles:
    
    Máximo de niveles guardados en cada lista.

  - muestra:
    
    Máximo de valores usados para enumerar niveles no declarados; los
    niveles ausentes de factores y metadatos se verifican sobre toda la
    columna.

  - proteger\_datos\_personales:
    
    Si se ocultan niveles concretos de columnas cuya clasificación
    activa protección automática. Véase `perfilar()`.

## Valor

Data frame S3 `clasificacion_variables`, editable y filtrable.

## Detalles

`metadatos` permite confirmar o corregir la propuesta con una tabla
editable. Debe contener `columna` y puede incluir `escala`, `rol`,
`confianza`, `confirmada`, `unidad` y una columna de lista `niveles`.
Las escalas válidas son nominal, ordinal, discreta, continua, binaria,
temporal y desconocida.

Los niveles declarados y observados se conservan como columnas de lista.
Los niveles ausentes son una observación, no prueba de error. Si la
evidencia de dato personal activa la protección, los niveles concretos
se protegen.

## Ver también

`analizar()`, `distribucion_valores()`, `proponer_modelo()`

## Ejemplos

``` r
d <- data.frame(
  prioridad = ordered(c("baja", "alta"), levels = c("baja", "media", "alta")),
  cantidad = c(1L, 2L)
)
clasificar_variables(d)
#>     columna tipo_almacenamiento tipo_implicito escala_propuesta       rol
#> 1 prioridad     factor-ordenado          texto          ordinal categoria
#> 2  cantidad              entero         entero          binaria indicador
#>   confianza
#> 1       1.0
#> 2       0.8
#>                                                                 evidencia
#> 1                               La clase ordered declara niveles y orden.
#> 2 Se observaron dos estados; pueden ser codigos y requieren confirmacion.
#>   confirmada unidad n_niveles_declarados n_niveles_observados
#> 1       TRUE   <NA>                    3                    2
#> 2      FALSE   <NA>                    0                    2
#>   n_niveles_ausentes niveles_muestreados niveles_truncados
#> 1                  1               FALSE             FALSE
#> 2                  0               FALSE             FALSE
#>                             metricas_sugeridas niveles_declarados
#> 1 ValoresPosiblesPorExtension; orden y niveles       baja, me....
#> 2          ValoresPosiblesPorExtension; NoNulo                   
#>   niveles_observados niveles_ausentes
#> 1         baja, alta            media
#> 2               1, 2                 
```
