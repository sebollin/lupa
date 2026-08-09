# Convertir un perfil a tibble

Método opcional para `tibble::as_tibble()`. Requiere que `tibble` esté
instalado.

## Uso

``` r
# Método S3 para la clase 'perfil'
as_tibble(x, ...)
```

## Argumentos

  - x:
    
    Objeto de clase `perfil`.

  - ...:
    
    Argumentos enviados a `tibble::as_tibble()`.

## Valor

Un `tibble` con una fila por columna.

## Ejemplos

``` r
perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
if (requireNamespace("tibble", quietly = TRUE)) {
  tibble::as_tibble(perfil)
}
#> # A tibble: 10 × 62
#>    columna tipo_declarado tipo_inferido proporcion_tipo_infe…¹     n n_faltantes
#>    <chr>   <chr>          <chr>                          <dbl> <int>       <int>
#>  1 id_per… doble          doble                          1        13           0
#>  2 cedula  texto          texto                          1        13           0
#>  3 fecha_… texto          fecha                          0.846    13           0
#>  4 sexo    texto          texto                          1        13           0
#>  5 ingreso doble          doble                          1        13           0
#>  6 depart… texto          texto                          1        13           0
#>  7 pais    texto          texto                          1        13           0
#>  8 correo  texto          texto                          1        13           0
#>  9 id_cop… doble          doble                          1        13           0
#> 10 id_tra… texto          identificador                  1        13           0
#> # ℹ abbreviated name: ¹​proporcion_tipo_inferido
#> # ℹ 56 more variables: prop_faltantes <dbl>, n_faltantes_disfrazados <int>,
#> #   n_faltantes_disfrazados_textuales <int>,
#> #   n_faltantes_disfrazados_numericos <int>, prop_faltantes_disfrazados <dbl>,
#> #   n_faltantes_totales <int>, prop_faltantes_totales <dbl>, n_distintos <int>,
#> #   tasa_distintos <dbl>, moda <chr>, frecuencia_moda <int>,
#> #   longitud_minima <dbl>, longitud_maxima <dbl>, longitud_media <dbl>, …
```
