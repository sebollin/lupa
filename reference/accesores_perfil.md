# Leer un perfil sin depender de su forma

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
y
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
devuelven objetos con formas distintas, y eso convertía la lectura en un
acertijo: `perfil$general$filas` funciona sobre la salida en memoria y
devuelve `NULL` sobre la salida DBI, donde el conteo vive en
`resumen_tabla$meta$filas`. Un `NULL` silencioso en un guion de medición
es la peor forma de fallar: no avisa, y lo que sigue calcula sobre nada.

## Usage

``` r
hallazgos(x, ...)

columnas(x, ...)

cobertura(x, ...)

n_filas(x, ...)

sql_perfil(x, ...)
```

## Arguments

- x:

  Objeto de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  o
  [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md).

- ...:

  Sin uso, para compatibilidad de métodos.

## Value

`hallazgos()` devuelve un `data.frame` con una fila por hallazgo;
`columnas()`, un `data.frame` con una fila por columna de la tabla
perfilada y sus métricas y diagnósticos; `cobertura()` devuelve la tabla
de diagnósticos no evaluados; `n_filas()` devuelve el conteo de filas
del alcance, o `NA` con su motivo cuando el objeto no lo conoce. El
significado de cada campo de `columnas()` se explica por familias en los
detalles de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

## Details

Estos accesores no agregan información: le ponen un solo nombre a la que
ya está. La forma concreta de cada objeto sigue disponible y
documentada.

**Lo que no hay, no se inventa.** Un
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
sin muestra leída no tiene hallazgos por fila, y `hallazgos()` devuelve
una tabla vacía **con su aviso**, no una tabla que aparente que se midió
y no había nada.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)

## Examples

``` r
perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
nrow(hallazgos(perfil))
#> [1] 23
n_filas(perfil)
#> [1] 13
nrow(cobertura(perfil))
#> [1] 3
```
