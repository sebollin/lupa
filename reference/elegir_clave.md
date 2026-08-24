# Elegir la clave entre las sugeridas

Muestra las candidatas de
[`sugerir_clave()`](https://sebollin.github.io/lupa/reference/sugerir_clave.md)
numeradas, con el motivo de cada una, y devuelve la que se elija para
pasarla al argumento `clave` de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
La última opción permite escribir un nombre que no esté en la lista, o
varios separados por coma para una clave compuesta.

## Usage

``` r
elegir_clave(datos, maximo = 5L, umbral_casi = 0.95)
```

## Arguments

- datos:

  Tabla a examinar.

- maximo:

  Cuántas sugerencias devolver como máximo.

- umbral_casi:

  Proporción de valores distintos a partir de la cual una columna que
  repite se informa igual, como candidata con duplicados.

## Value

El nombre —o los nombres— de la clave elegida, o `NULL` si no se eligió
ninguna o la sesión no es interactiva.

## Details

**En una sesión no interactiva no pregunta**: devuelve `NULL` y avisa
qué habría ofrecido. Un guion que corre solo no puede quedarse esperando
una respuesta, y elegir por su cuenta sería exactamente lo que esta
función existe para no hacer.

## See also

[`sugerir_clave()`](https://sebollin.github.io/lupa/reference/sugerir_clave.md),
y el argumento `clave` de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

## Examples

``` r
personas <- data.frame(id_persona = 1:3, edad = c(30, 41, 25))
# En una sesion interactiva pregunta; aqui devuelve NULL y dice que ofreceria.
elegir_clave(personas)
#> ℹ Sesión no interactiva: no se pregunta. Se habría ofrecido `id_persona`, `edad`. Usar `sugerir_clave()` y pasar la elegida a `perfilar(clave = ...)`.
#> NULL
```
