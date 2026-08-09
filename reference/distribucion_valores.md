# Distribuciones de valores y cuantiles por columna

Resume frecuencias sin conservar una tabla completa de alta
cardinalidad. Cada columna se limita a `max_valores`; `alcance` declara
cuántos valores se analizaron, cuántos distintos se observaron y si hubo
muestreo o truncamiento. Los cuantiles se calculan sólo para números
ordinarios finitos.

## Uso

``` r
distribucion_valores(
  datos,
  perfil = NULL,
  max_valores = 20L,
  probabilidades = c(0, 0.25, 0.5, 0.75, 1),
  muestra = 1e+05,
  proteger_datos_personales = TRUE
)
```

## Argumentos

  - datos:
    
    Tabla que se desea examinar.

  - perfil:
    
    Perfil opcional de los mismos datos; evita repetir la clasificación
    de posibles datos personales.

  - max\_valores:
    
    Máximo de valores mostrados por columna.

  - probabilidades:
    
    Probabilidades de los cuantiles, en `[0, 1]`.

  - muestra:
    
    Máximo de filas por columna; `Inf` desactiva el muestreo.

  - proteger\_datos\_personales:
    
    Si se ocultan valores de columnas cuya clasificación activa
    protección automática. Véase `perfilar()`.

## Valor

Objeto `distribuciones_perfil`, una lista con data frames `frecuencias`,
`cuantiles` y `alcance`. Todas las proporciones están en `[0, 1]`.

## Detalles

Cuando una columna tiene evidencia suficiente para activar la protección
de datos personales, sus frecuencias y niveles se conservan pero el
valor concreto se reemplaza. Los cuantiles mantienen sus filas y
probabilidades, pero `valor` queda en `NA` y `estado` informa
`"valor_protegido"`: un cuantil, especialmente en tablas pequeñas, puede
coincidir exactamente con una observación. Esta protección es
independiente de la usada al construir el perfil.

## Ver también

`perfilar()`, `analizar()`, `clasificar_variables()`

## Ejemplos

``` r
d <- data.frame(grupo = c("A", "A", "B"), valor = c(1, 2, 10))
distribucion_valores(d)
#> $frecuencias
#>   columna rango valor frecuencia proporcion
#> 1   grupo     1     A          2  0.6666667
#> 2   grupo     2     B          1  0.3333333
#> 3   valor     1     1          1  0.3333333
#> 4   valor     2     2          1  0.3333333
#> 5   valor     3    10          1  0.3333333
#> 
#> $cuantiles
#>   columna probabilidad valor n_analizados muestreado    estado
#> 1   valor         0.00   1.0            3      FALSE calculado
#> 2   valor         0.25   1.5            3      FALSE calculado
#> 3   valor         0.50   2.0            3      FALSE calculado
#> 4   valor         0.75   6.0            3      FALSE calculado
#> 5   valor         1.00  10.0            3      FALSE calculado
#> 
#> $alcance
#>   columna n_total n_analizados n_distintos_muestra n_mostrados muestreado
#> 1   grupo       3            3                   2           2      FALSE
#> 2   valor       3            3                   3           3      FALSE
#>   truncado protegida    estado
#> 1    FALSE     FALSE calculada
#> 2    FALSE     FALSE calculada
#> 
#> attr(,"class")
#> [1] "distribuciones_perfil"
```
