# Granularidades y transiciones de agregación

`granularidades()` declara los diez niveles del marco, y los diez se
miden. Los cuatro de arriba —colección, conjunto de colecciones,
organización y conjunto de organizaciones— sólo cuando el usuario
**declara la frontera**: qué tablas componen una colección, qué bases un
conjunto, qué colecciones una organización, qué organizaciones un
conjunto. `lupa` no infiere ninguna de las cuatro, porque ninguna está
en los datos.

## Usage

``` r
granularidades()

transiciones_granularidad()
```

## Value

Data frames con niveles o aristas del grafo de granularidad.

## Details

Que estén implementadas no obliga a usarlas. Un análisis que no tiene
una organización detrás se detiene donde corresponda; los niveles
superiores existen para quien los necesita.

`transiciones_granularidad()` devuelve el grafo dirigido de
agregaciones. La transición `instanciaAtributo` a `instanciaEntidad` se
incorpora porque el propio marco la usa aunque no aparezca en su tabla
no exhaustiva.

## See also

[`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md)

## Examples

``` r
granularidades()
#>    nivel           granularidad           relacional implementada
#> 1      1      instanciaAtributo                celda         TRUE
#> 2      2               atributo              columna         TRUE
#> 3      3      conjuntoAtributos conjunto de columnas         TRUE
#> 4      4       instanciaEntidad                tupla         TRUE
#> 5      5                entidad                tabla         TRUE
#> 6      6      conjuntoEntidades   conjunto de tablas         TRUE
#> 7      7              coleccion        base de datos         TRUE
#> 8      8    conjuntoColecciones                 <NA>         TRUE
#> 9      9           organizacion                 <NA>         TRUE
#> 10    10 conjuntoOrganizaciones                 <NA>         TRUE
transiciones_granularidad()
#>              origen                destino                fuente
#> 1 instanciaAtributo               atributo                 marco
#> 2 instanciaAtributo       instanciaEntidad extension_documentada
#> 3  instanciaEntidad                entidad                 marco
#> 4          atributo                entidad                 marco
#> 5           entidad      conjuntoEntidades                 marco
#> 6           entidad              coleccion                 marco
#> 7         coleccion    conjuntoColecciones                 marco
#> 8         coleccion           organizacion                 marco
#> 9      organizacion conjuntoOrganizaciones                 marco
```
