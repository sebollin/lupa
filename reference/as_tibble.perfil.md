# Convertir un perfil a tibble

Método opcional para
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).
Requiere que `tibble` esté instalado.

## Usage

``` r
# S3 method for class 'perfil'
as_tibble(x, ...)
```

## Arguments

- x:

  Objeto de clase `perfil`.

- ...:

  Argumentos enviados a
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).

## Value

Un `tibble` con una fila por columna.

## Examples

``` r
perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
if (requireNamespace("tibble", quietly = TRUE)) {
  tibble::as_tibble(perfil)
}
#> # A tibble: 10 × 99
#>    columna          tipo_declarado tipo_inferido proporcion_tipo_inferido
#>    <chr>            <chr>          <chr>                            <dbl>
#>  1 id_persona       doble          doble                            1    
#>  2 cedula           texto          texto                            1    
#>  3 fecha_nacimiento texto          fecha                            0.846
#>  4 sexo             texto          texto                            1    
#>  5 ingreso          doble          doble                            1    
#>  6 departamento     texto          texto                            1    
#>  7 pais             texto          texto                            1    
#>  8 correo           texto          texto                            1    
#>  9 id_copia         doble          doble                            1    
#> 10 id_tramite       texto          identificador                    1    
#> # ℹ 95 more variables: n_filas_analizadas_tipo <int>,
#> #   muestreado_tipo_inferido <lgl>, n <int>, n_faltantes <int>,
#> #   prop_faltantes <dbl>, n_faltantes_disfrazados <int>,
#> #   n_faltantes_disfrazados_textuales <int>,
#> #   n_faltantes_disfrazados_numericos <int>, prop_faltantes_disfrazados <dbl>,
#> #   n_faltantes_totales <int>, prop_faltantes_totales <dbl>, n_distintos <int>,
#> #   tasa_distintos <dbl>, secuencia_entera_densa <lgl>, …
```
