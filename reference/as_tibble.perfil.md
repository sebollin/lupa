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
#> # A tibble: 10 × 110
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
#> # ℹ 106 more variables: n_filas_analizadas_tipo <int>,
#> #   muestreado_tipo_inferido <lgl>, n <int>, n_aplicables <int>,
#> #   n_no_aplica <int>, n_aplicabilidad_indeterminada <int>,
#> #   n_presentes_fuera_de_aplicabilidad <int>, n_faltantes <int>,
#> #   prop_faltantes <dbl>, n_faltantes_disfrazados <int>,
#> #   n_faltantes_disfrazados_textuales <int>,
#> #   n_faltantes_disfrazados_numericos <int>, …
```
