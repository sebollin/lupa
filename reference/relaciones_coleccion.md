# Buscar claves foráneas candidatas entre pares declarados

Corre
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
sobre los pares de tablas que se declaren, y devuelve las relaciones
candidatas junto con la cobertura de los pares que no se pudieron
comparar.

## Usage

``` r
relaciones_coleccion(coleccion, pares, muestra = 10000, umbral_cobertura = 0.9)
```

## Arguments

- coleccion:

  Objeto creado por
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md).

- pares:

  Data frame con columnas `tabla_1` y `tabla_2`.

- muestra:

  Filas traídas por tabla para comparar.

- umbral_cobertura:

  Cobertura mínima para informar una relación.

## Value

Objeto `relaciones_coleccion` con `relaciones`, `cobertura_pares` y
`meta`.

## Details

**Los pares se declaran, igual que la frontera.** Con mil tablas hay
casi un millón de pares dirigidos, así que explorar todos no es una
opción cara sino una opción imposible.
[`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md)
permite verlo antes.

Cada par se compara sobre una muestra de filas de cada tabla, y el
resultado declara ese alcance: una relación candidata sobre una muestra
**no es una clave foránea comprobada**, es un indicio que hay que
confirmar contra el diccionario de datos.

## See also

[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md),
[`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md),
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
