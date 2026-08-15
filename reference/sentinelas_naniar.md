# Sentinelas numéricos publicados por [naniar](https://github.com/njtierney/naniar)

Vector para solicitar explícitamente la lista numérica completa
publicada por [naniar](https://github.com/njtierney/naniar). Sus autores
incluyen a [Nicholas Tierney](https://github.com/njtierney), [Di
Cook](https://github.com/dicook), [Miles
McBain](https://github.com/milesmcbain) y [Colin
Fay](https://github.com/ColinFay). Incluye `66`, `77`, `88` y `9999`,
que pueden ser edades, códigos o años legítimos y por eso no se aplican
de forma predeterminada. Se usa como
`perfilar(datos, sentinelas_numericos = sentinelas_naniar)`. Tanto este
vector como las cadenas de ausencia incorporadas en el paquete están
congelados con referencia a
[naniar](https://github.com/njtierney/naniar) 1.1.0; no cambian según la
versión instalada.

## Usage

``` r
sentinelas_naniar
```

## Format

Vector numérico de ocho elementos.

## Source

`naniar::common_na_numbers`, versión 1.1.0. Véase el [repositorio de
naniar](https://github.com/njtierney/naniar).

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)

## Examples

``` r
sentinelas_naniar
#> [1]    -9   -99  -999 -9999  9999    66    77    88
datos <- data.frame(codigo = c(1, 66, 9999))
perfil <- perfilar(
  datos, sentinelas_numericos = sentinelas_naniar,
  analizar_dependencias = FALSE
)
perfil$columnas[, c("columna", "n_faltantes_disfrazados")]
#>   columna n_faltantes_disfrazados
#> 1  codigo                       2
```
