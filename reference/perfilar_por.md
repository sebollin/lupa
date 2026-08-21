# Perfilar una tabla por grupos de filas

Aplica
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) a
cada grupo de filas por separado y devuelve los hallazgos de todos los
grupos en una sola tabla. Es la respuesta al formato largo: una tabla
donde cada fila describe un atributo distinto no es una tabla, son
muchas apiladas, y perfilarla como si fuera una sola mezcla dominios que
no tienen nada que ver entre sí.

## Usage

``` r
perfilar_por(datos, por, clave = NULL, min_filas = 30L, ...)
```

## Arguments

- datos:

  Data frame a perfilar.

- por:

  Nombre de una columna atómica cuyos valores definen los grupos. Los
  ausentes forman un grupo propio.

- clave:

  Nombres de columnas de identidad que se conservan en cada grupo aunque
  estén enteramente ausentes. Importa: sin la clave de entidad, el
  diagnóstico de filas duplicadas informa como duplicada cada repetición
  del valor del atributo.

- min_filas:

  Grupos con menos filas que este número no se perfilan y se declaran en
  la cobertura. El valor por omisión evita conclusiones sobre grupos
  donde ningún diagnóstico tiene soporte.

- ...:

  Argumentos enviados a
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  para cada grupo.

## Value

Data frame de clase `hallazgos_por_grupo` con las columnas de
`hallazgos` de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
precedidas por `grupo` y `n_filas_grupo`. El atributo `cobertura_grupos`
declara los grupos no perfilados y las columnas descartadas por grupo.

## Details

Dentro de cada grupo se descartan las columnas enteramente ausentes
antes de perfilar. En un modelo entidad-atributo-valor bien formado eso
deja viva exactamente la columna de valor que corresponde al atributo
del grupo, y es lo que evita informar como falta lo que es la forma del
dato. El descarte se declara en la cobertura.

La función no adivina cuál es la columna de agrupación: la declara quien
conoce el dato, igual que
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) no
adivina claves ni jerarquías.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)

## Examples

``` r
largo <- data.frame(
  entidad = rep(1:40, each = 2),
  atributo = rep(c("pais", "edad"), 40),
  valor = c(rbind(sample(c("UY", "AR"), 40, TRUE), as.character(20:59)))
)
hallazgos <- perfilar_por(largo, "atributo", clave = "entidad", min_filas = 10)
head(hallazgos[, c("grupo", "columna", "tipo_hallazgo")])
#>   grupo columna         tipo_hallazgo
#> 1  pais entidad posible_identificador
#> 2  edad entidad posible_identificador
#> 3  edad   valor posible_identificador
```
