# Declarar una organización y las colecciones que le pertenecen

Las granularidades novena y décima del marco —una organización y un
conjunto de organizaciones— necesitan un dato que **no está en los
datos**: qué bases pertenecen a qué organismo. `lupa` no lo adivina, así
que lo declara quien lo sabe, con el mismo mecanismo que ya usa el
conjunto de colecciones.

## Usage

``` r
organizacion(nombre, colecciones)
```

## Arguments

- nombre:

  Nombre de la organización. Es su identidad dentro de un conjunto de
  organizaciones.

- colecciones:

  Colecciones que le pertenecen: un vector de nombres, o una lista de
  objetos de
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
  o
  [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md).
  Si la lista tiene nombres, esos nombres mandan sobre el del objeto.

## Value

Objeto S3 `organizacion_lupa` con `nombre`, `declaradas` y
`n_declaradas`.

## Details

**Es opcional.** Un análisis de calidad no siempre tiene una
organización detrás, y nada obliga a pasar por estos niveles: existen
para quien los necesita. Sin declaración,
[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md) a
`"organizacion"` se niega y explica cómo declararla, que es distinto de
inventar una frontera que nadie nombró.

No pide una conexión. Una colección es una cosa viva —tablas de un
motor—, pero una organización es un enunciado *sobre* colecciones, y
puede reunir colecciones medidas en momentos distintos o contra motores
distintos.

## See also

[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md),
[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md),
[`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md)

## Examples

``` r
organismo <- organizacion("Organismo A", c("padron", "tramites"))
organismo
#> Organización declarada: Organismo A
#> 2 colecciones: "padron" and "tramites"
#> La frontera es declarada: `lupa` no infiere a qué organismo pertenece una base.
organismo$declaradas
#> [1] "padron"   "tramites"

# Cuando los nombres siguen una convencion, la agrupacion se deriva de ella
# y despues se declara. `lupa` no aplica la regla por su cuenta -deducir el
# dueno de una base a partir de su nombre es una inferencia, y la frontera
# se declara-, pero tampoco obliga a escribir la lista a mano: la regla la
# pone quien conoce la convencion, y con la forma que quiera.
esquemas <- c("org_a_padron", "org_a_tramites", "org_b_registro", "cruce")
sigla <- ifelse(grepl("^org_", esquemas), sub("_[^_]+$", "", esquemas), NA)
partes <- split(esquemas[!is.na(sigla)], sigla[!is.na(sigla)])
organismos <- Map(organizacion, names(partes), partes)

# Y lo que la convencion no alcanza -aca, una base de un integrador, que no
# tiene dueno unico- se declara aparte, en vez de quedar afuera en silencio.
organismos[["integrador"]] <- organizacion("integrador", esquemas[is.na(sigla)])
vapply(organismos, function(o) o$n_declaradas, integer(1))
#>      org_a      org_b integrador 
#>          2          1          1 
```
