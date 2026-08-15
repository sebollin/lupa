# Detectar asociaciones entre columnas

Calcula Pearson entre numéricas, V de Cramér entre categóricas y eta
cuadrado entre una categórica y una numérica. Las tres medidas se
informan en `[0, 1]`: Pearson usa su valor absoluto. La tabla declara el
método, soporte y posible muestreo; no presenta significancia
estadística.

## Usage

``` r
detectar_asociaciones(
  datos,
  dependencias = NULL,
  umbral = 0.3,
  muestra = 10000,
  max_columnas = 50L,
  max_niveles = 50L,
  max_pares = 500L
)
```

## Arguments

- datos:

  Tabla que se desea examinar.

- dependencias:

  Resultado opcional de
  [`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md).

- umbral:

  Valor mínimo en `[0, 1]` que se informa.

- muestra:

  Máximo común de filas; `Inf` desactiva el muestreo.

- max_columnas:

  Máximo de columnas analizables.

- max_niveles:

  Máximo de niveles para tratar una columna como categórica.

- max_pares:

  Máximo de asociaciones devueltas después de ordenar.

## Value

Data frame S3 `asociaciones_columnas`. Sus atributos declaran filas,
columnas y pares examinados, omisiones por dependencia y truncamiento.

## Details

Se descartan constantes, fechas, listas, categóricas de cardinalidad
alta y columnas posteriores a `max_columnas` antes de construir pares.
Las dependencias funcionales exactas recibidas en `dependencias` no se
repiten como asociaciones.

## See also

[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md),
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)

## Examples

``` r
d <- data.frame(x = 1:20, y = 2 * (1:20), grupo = rep(c("A", "B"), 10))
detectar_asociaciones(d, umbral = 0)
#>   columna_1 columna_2   tipo_1     tipo_2           metodo
#> 1         x         y numerica   numerica pearson_absoluto
#> 2         x     grupo numerica categorica             eta2
#> 3         y     grupo numerica categorica             eta2
#>                                                                              supuesto
#> 1 Las columnas numericas se tratan como cuantitativas; la escala no queda confirmada.
#> 2            La columna numerica se trata como cuantitativa y la otra como categoria.
#> 3            La columna numerica se trata como cuantitativa y la otra como categoria.
#>    asociacion n_pares
#> 1 1.000000000      20
#> 2 0.007518797      20
#> 3 0.007518797      20
```
