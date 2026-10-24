# Distribuciones de valores y cuantiles por columna

Resume frecuencias sin conservar una tabla completa de alta
cardinalidad. Cada columna se limita a `max_valores`; `alcance` declara
cuántos valores se analizaron, cuántos distintos se observaron y si hubo
muestreo o truncamiento. Los cuantiles se calculan sólo para números
ordinarios finitos.

## Usage

``` r
distribucion_valores(
  datos,
  perfil = NULL,
  max_valores = 20L,
  probabilidades = c(0, 0.25, 0.5, 0.75, 1),
  muestra = 1e+05,
  proteger_datos_personales = TRUE,
  columnas_personales = character(),
  .universo = NULL
)
```

## Arguments

- datos:

  Tabla que se desea examinar.

- perfil:

  Perfil opcional de los mismos datos; evita repetir la clasificación de
  posibles datos personales. Si el perfil declaró `columnas_codigo`,
  esas columnas publican en `cuantiles` sólo los extremos —probabilidad
  0 y 1, el tramo de códigos—; los intermedios quedan en `NA` con
  `estado = "codigo_declarado"`, como `mediana` en el perfil.

- max_valores:

  Máximo de valores mostrados por columna.

- probabilidades:

  Probabilidades de los cuantiles, en `[0, 1]`.

- muestra:

  Máximo de filas por columna; `Inf` desactiva el muestreo.

- proteger_datos_personales:

  Si se ocultan valores de columnas cuya clasificación activa protección
  automática. Véase
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
  Con `perfil`, se respeta la clasificación de ese perfil —incluido lo
  que su usuario declaró—; sin `perfil`, la clasificación se rehace aquí
  por léxico y por forma, y lo declarado entra por
  `columnas_personales`.

- columnas_personales:

  Columnas que traen datos personales, declaradas con la misma forma que
  acepta
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
  nombres de columna, o un vector con nombre donde el nombre es la
  columna y el valor es el tipo. **Es el único camino cuando no se pasa
  `perfil`**: sin perfil, la clasificación corre por léxico y por forma,
  así que una columna que sólo quien la conoce sabe personal —una edad,
  un legajo interno— se publicaría entera. Sólo tiene efecto con
  `proteger_datos_personales = TRUE`.

- .universo:

  Uso interno de
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md):
  el universo que declara su `argumentos_perfil` —`aplicabilidad`,
  `columnas_opcionales` y `sentinelas_numericos`—. Con él, lo que queda
  fuera del universo no entra en `frecuencias` ni en `cuantiles`, y un
  centinela declarado sale de los `cuantiles` como sale de `minimo`,
  `mediana` y `maximo` del perfil; en `frecuencias` sigue contando, como
  en `n_distintos` y `moda`.

## Value

Objeto `distribuciones_perfil`, una lista con data frames `frecuencias`,
`cuantiles` y `alcance`. Todas las proporciones están en `[0, 1]`.

## Details

**Las proporciones de `frecuencias` se calculan sobre los valores que se
pudieron analizar, no sobre la columna entera**, y `alcance` dice
cuántos fueron: `n_total` contra `n_analizados`. Cuando alguno no se
pudo comparar —bytes inválidos, sobre todo— el `estado` de esa columna
es `calculada_parcial` en vez de `calculada`, para que un resultado
parcial no se lea como completo. Un `NA` no cuenta como descarte: es una
ausencia declarada, no un valor que no se pudo analizar. Los otros
estados posibles son `sin_valores`, `sin_valores_analizables` y
`tipo_no_comparable`. La entrada es la de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
un `POSIXlt` se cuenta por su instante, como el `POSIXct` equivalente, y
el nivel `NA` de un factor es un faltante.

Cuando una columna tiene evidencia suficiente para activar la protección
de datos personales, sus frecuencias y niveles se conservan pero el
valor concreto se reemplaza. Los cuantiles mantienen sus filas y
probabilidades, pero `valor` queda en `NA` y `estado` informa
`"valor_protegido"`: un cuantil, especialmente en tablas pequeñas, puede
coincidir exactamente con una observación. Esta protección es
independiente de la usada al construir el perfil.

Hay clases que R declara numéricas y cuyo cuantil interpolado no se
puede calcular, porque su aritmética rechaza el producto por una
fracción —`lubridate::Period` es una—. Esas columnas **no interrumpen el
análisis**: conservan sus filas con `valor` en `NA` y el `estado`
`"no_interpolable:<clase>"`, que nombra la clase que no se pudo
interpolar. La condición se mide intentando el cálculo, no enumerando
clases, así que una clase nueva con la misma limitación queda cubierta
sin tocar el código.

Las frecuencias se cuentan **por valor, no por su escritura**: dos
dobles que coinciden en sus primeras 15 cifras —`0.1 + 0.2` y `0.3`,
`1e15` y `1e15 + 1`— son dos filas, como los cuenta
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md), y
cada una se escribe con las cifras que la devuelven a su valor. Un
instante se escribe en UTC y con el rótulo `UTC`, como los resúmenes de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
la etiqueta no depende de la zona de la sesión. A igual frecuencia, las
filas se ordenan por valor —el texto por sus bytes, los números como
números—, el mismo desempate que la moda de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
así, el recorte de `max_valores` no depende del orden de las filas de la
tabla.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
[`clasificar_variables()`](https://sebollin.github.io/lupa/reference/clasificar_variables.md)

## Examples

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
