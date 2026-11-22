# Convertir una propuesta de ausencias en la declaración por columna

Toma las filas de una propuesta de
[`detectar_tablas_codigos()`](https://sebollin.github.io/lupa/reference/detectar_tablas_codigos.md)
que el usuario marcó con `incluir = TRUE` y devuelve las listas que
reciben `sentinelas_numericos` y `cadenas_ausencia`: por tabla para
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
o, con `tabla`, por columna para
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
Cada código va al argumento que dice su fila: un número para una columna
que R lee como número, un texto para una de texto.

## Usage

``` r
ausencias_desde_propuesta(propuesta, tabla = NULL)
```

## Arguments

- propuesta:

  El componente `propuesta` de
  [`detectar_tablas_codigos()`](https://sebollin.github.io/lupa/reference/detectar_tablas_codigos.md),
  o el objeto entero.

- tabla:

  Identificador de una tabla de la colección. Con él, la salida son las
  listas por columna de esa tabla.

## Value

Lista con `sentinelas_numericos` y `cadenas_ausencia`, cada una presente
solo si tiene alguna entrada, lista para
[`do.call()`](https://rdrr.io/r/base/do.call.html).

## Details

En una columna con entrada propia, `sentinelas_numericos` reemplaza la
lista por omisión, como el vector la reemplaza en todas: en una columna
gobernada por una tabla de códigos, la tabla dice qué significa cada
valor y uno que no está es un huérfano, no un centinela. Las columnas
sin filas incluidas no aparecen y reciben el valor por omisión.

## See also

[`detectar_tablas_codigos()`](https://sebollin.github.io/lupa/reference/detectar_tablas_codigos.md),
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
