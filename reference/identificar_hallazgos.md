# Identificar los hallazgos de un perfil entre corridas

Devuelve una fila por hallazgo con su **identidad estable**,
`id_hallazgo`: la tabla, el grupo (en el resultado de
[`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md)),
la columna y el tipo de hallazgo, separados por `|`. No depende del
orden de las filas ni de un número de llamada, así que el mismo hallazgo
tiene el mismo id en cada corrida y en tablas distintas los ids no
chocan. Dos hallazgos del mismo tipo en la misma columna se distinguen
con un sufijo `|#k` que sale del orden de sus bytes, no del orden de
aparición.

## Usage

``` r
identificar_hallazgos(x, nombre_tabla = NULL)
```

## Arguments

- x:

  Resultado de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
  [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
  o
  [`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md).

- nombre_tabla:

  Nombre de la tabla que se perfiló, para la identidad. Es obligatorio
  con el resultado de
  [`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md),
  que no guarda el nombre: sin él, dos tablas con los mismos grupos y
  columnas compartirían ids. En los demás objetos se toma del objeto
  (`nombre` de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  la tabla calificada con su esquema en
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  y las colecciones) y sólo hace falta para cambiarlo.

## Value

Un `data.frame` con `id_hallazgo`, `tabla`, `grupo` (`NA` fuera de
[`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md)),
`columna`, `tipo_hallazgo`, `severidad` y `n_afectados`. Si `x` trae
decisiones aplicadas, agrega las columnas de estado de
[`aplicar_decisiones()`](https://sebollin.github.io/lupa/reference/aplicar_decisiones.md).

## Details

Con columnas de nombre repetido, el sufijo es la posición de la columna
entre las homónimas, que no cambia si se reordenan otras columnas.

Es lo que une el archivo de decisiones con la corrida: ver
[`decidir_hallazgos()`](https://sebollin.github.io/lupa/reference/decidir_hallazgos.md)
y
[`aplicar_decisiones()`](https://sebollin.github.io/lupa/reference/aplicar_decisiones.md).

## See also

[`decidir_hallazgos()`](https://sebollin.github.io/lupa/reference/decidir_hallazgos.md),
[`aplicar_decisiones()`](https://sebollin.github.io/lupa/reference/aplicar_decisiones.md)

## Examples

``` r
perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
head(identificar_hallazgos(perfil))
#>                                                     id_hallazgo
#> 1       datos_administrativos||id_persona|posible_identificador
#> 2               datos_administrativos||cedula|alta_cardinalidad
#> 3           datos_administrativos||cedula|faltantes_disfrazados
#> 4     datos_administrativos||fecha_nacimiento|alta_cardinalidad
#> 5 datos_administrativos||fecha_nacimiento|faltantes_disfrazados
#> 6 datos_administrativos||fecha_nacimiento|formatos_fecha_mixtos
#>                   tabla grupo          columna         tipo_hallazgo  severidad
#> 1 datos_administrativos  <NA>       id_persona posible_identificador         ok
#> 2 datos_administrativos  <NA>           cedula     alta_cardinalidad sospechoso
#> 3 datos_administrativos  <NA>           cedula faltantes_disfrazados      error
#> 4 datos_administrativos  <NA> fecha_nacimiento     alta_cardinalidad sospechoso
#> 5 datos_administrativos  <NA> fecha_nacimiento faltantes_disfrazados      error
#> 6 datos_administrativos  <NA> fecha_nacimiento formatos_fecha_mixtos      error
#>   n_afectados
#> 1           1
#> 2           1
#> 3           1
#> 4           1
#> 5           1
#> 6           5
```
