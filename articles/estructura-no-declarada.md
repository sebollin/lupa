# 4. Estructura no declarada

Una entrega puede llegar sin diccionario, sin claves y sin relaciones
declaradas. ¿Qué estructura cumplen de hecho los datos recibidos, y qué
parte de esa estructura todavía hay que confirmar con quien conoce el
sistema?

> **En esta guía:**
>
> - buscamos una clave candidata con
>   [`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md);
> - comparamos dos tablas con
>   [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
>   y leemos su cardinalidad;
> - encontramos una dependencia entre columnas con
>   [`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md);
> - separamos lo que se cumple en estos datos de lo que es una regla del
>   sistema.

## Dos tablas que se pueden seguir fila por fila

Los conjuntos incluidos en `lupa` sirven para examinar columnas, pero no
contienen una clave exacta: en `datos_operativos`, `id_evento` tiene 12
valores distintos en 13 filas. Sobre ellos,
[`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md)
y
[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md)
devolverían tablas sin filas.

Para ver una clave, una relación y una dependencia construimos dos
tablas chicas: una de personas y otra de trámites. Cada conclusión se
puede contrastar con las filas impresas.

``` r

library(lupa)

personas <- data.frame(
  id_persona = 101:108,
  nombre = rep(c("Ana", "Bruno", "Carla", "Diego"), each = 2),
  ciudad = rep(c("Montevideo", "Salto", "Belén", "Melo"), each = 2),
  departamento = rep(
    c("Montevideo", "Salto", "Salto", "Cerro Largo"), each = 2
  ),
  stringsAsFactors = FALSE
)

tramites <- data.frame(
  id_tramite = 1001:1010,
  id_persona = c(101, 101, 102, 103, 104, 104, 105, 106, 107, 108),
  tipo = rep(c("alta", "renovación"), 5),
  stringsAsFactors = FALSE
)

personas
#>   id_persona nombre     ciudad departamento
#> 1        101    Ana Montevideo   Montevideo
#> 2        102    Ana Montevideo   Montevideo
#> 3        103  Bruno      Salto        Salto
#> 4        104  Bruno      Salto        Salto
#> 5        105  Carla      Belén        Salto
#> 6        106  Carla      Belén        Salto
#> 7        107  Diego       Melo  Cerro Largo
#> 8        108  Diego       Melo  Cerro Largo
tramites
#>    id_tramite id_persona       tipo
#> 1        1001        101       alta
#> 2        1002        101 renovación
#> 3        1003        102       alta
#> 4        1004        103 renovación
#> 5        1005        104       alta
#> 6        1006        104 renovación
#> 7        1007        105       alta
#> 8        1008        106 renovación
#> 9        1009        107       alta
#> 10       1010        108 renovación
```

## Una clave candidata

`id_persona` no falta y no se repite en `personas`. Limitamos la
búsqueda a columnas simples (`max_combinacion = 1`) para que la salida
responda sólo esa pregunta:

``` r

claves <- detectar_claves(personas, max_combinacion = 1)
claves[, c(
  "columnas", "n_columnas", "n_filas", "casi_clave", "unicidad_exacta"
)]
#>     columnas n_columnas n_filas casi_clave unicidad_exacta
#> 1 id_persona          1       8      FALSE            TRUE
```

`nombre`, `ciudad` y `departamento` se repiten, así que no aparecen. Que
una columna identifique cada fila en esta entrega la vuelve candidata,
no clave: una magnitud que no repite valores también pasaría. Declarar
la clave al perfilar, con `perfilar(clave = ...)`, es lo que permite ir
de un hallazgo a la fila en el sistema de origen; eso se explica en [2.
Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md).

## Una relación entre dos tablas

`id_persona` también está en `tramites`, y ahí se repite: una persona
puede tener más de un trámite.
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
recibe las dos columnas de identificación y compara sus valores:

``` r

detectar_relaciones(
  personas["id_persona"], tramites["id_persona"], muestra = Inf
)
#>   columna_tabla1 columna_tabla2 cardinalidad n_valores_comunes cobertura_tabla1_en_tabla2
#> 1     id_persona     id_persona          1:m                 8                          1
#>   cobertura_tabla2_en_tabla1 motivo_poda
#> 1                          1        <NA>
```

La cardinalidad es 1:m, uno a muchos. Las dos coberturas valen 1 porque
todos los identificadores de cada lado aparecen en el otro: no hay
trámites de personas que no estén en `personas`, ni personas sin
trámites. `muestra = Inf` deja explícito que se comparan todos los
valores.

## Una dependencia dentro de una tabla

En `personas`, cada ciudad observada pertenece a un solo departamento:
conocer la ciudad determina el departamento. Al revés no vale, porque
Salto y Belén comparten departamento. Bajamos el mínimo de observaciones
a cuatro porque la tabla tiene ocho filas:

``` r

dependencias <- detectar_dependencias(
  personas[c("ciudad", "departamento")],
  muestra = Inf,
  min_observaciones = 4
)
dependencias
#>   determinante  dependiente cumplimiento n_evaluados n_grupos n_violaciones exacta
#> 1       ciudad departamento            1           8        4             0   TRUE
#>   evidencia
#> 1
```

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
busca las mismas dependencias por su cuenta
(`analizar_dependencias = TRUE`, el valor por omisión) y las deja en
`perfil$dependencias`. Una dependencia exacta es además lo que permite
imputar un valor faltante desde los propios datos, como se ve en [6.
Limpiar con un
plan](https://sebollin.github.io/lupa/articles/limpiar-con-un-plan.md).

## Lo que se cumple no es una regla

Nada de esto modifica los datos ni declara el modelo. La clave es
*candidata*, la cardinalidad describe estas dos tablas y la dependencia
se cumple en estas ocho filas. Una entrega futura puede contradecirlas;
la documentación del sistema es la que puede confirmarlas como reglas.

## Resumen

- [`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md)
  encuentra las columnas que identifican cada fila en esta entrega: son
  candidatas, no claves.
- [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
  compara dos columnas de identificación y publica la cardinalidad y la
  cobertura de cada lado.
- [`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md)
  encuentra columnas que determinan a otras;
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  las busca por su cuenta.
- Lo que los datos cumplen hoy lo confirma la documentación del sistema,
  no el paquete.

## Paso siguiente

[5. El vacío por
diseño](https://sebollin.github.io/lupa/articles/vacio-por-diseno.md)
([`vignette("vacio-por-diseno")`](https://sebollin.github.io/lupa/articles/vacio-por-diseno.md)):
por qué una celda vacía puede ser correcta, y cómo se le dice al
paquete.
