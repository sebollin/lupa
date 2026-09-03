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

Un `tibble` con una fila por columna del perfil y las mismas variables
que `x$columnas`: es la tabla de columnas del perfil, no el perfil
entero -los hallazgos, la cobertura y los metadatos no viajan-.

## Examples

``` r
perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
if (requireNamespace("tibble", quietly = TRUE)) {
  tibble::as_tibble(perfil)
}
#> # A tibble: 10 × 111
#>    columna          tipo_declarado tipo_inferido estado_tipo_inferido
#>    <chr>            <chr>          <chr>         <chr>               
#>  1 id_persona       doble          doble         NA                  
#>  2 cedula           texto          texto         NA                  
#>  3 fecha_nacimiento texto          fecha         confirmado          
#>  4 sexo             texto          texto         NA                  
#>  5 ingreso          doble          doble         NA                  
#>  6 departamento     texto          texto         NA                  
#>  7 pais             texto          texto         NA                  
#>  8 correo           texto          texto         NA                  
#>  9 id_copia         doble          doble         NA                  
#> 10 id_tramite       texto          identificador NA                  
#> # ℹ 107 more variables: proporcion_tipo_inferido <dbl>,
#> #   n_filas_analizadas_tipo <int>, muestreado_tipo_inferido <lgl>, n <int>,
#> #   n_aplicables <int>, n_no_aplica <int>, n_aplicabilidad_indeterminada <int>,
#> #   n_presentes_fuera_de_aplicabilidad <int>, n_faltantes <int>,
#> #   prop_faltantes <dbl>, n_faltantes_disfrazados <int>,
#> #   n_faltantes_disfrazados_textuales <int>,
#> #   n_faltantes_disfrazados_numericos <int>, …
```
