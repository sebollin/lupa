# Agregar medidas entre granularidades

Aplica exactamente una de las cuatro agregaciones del marco: `ratio`,
`ratio_umbral`, `promedio` o `promedio_ponderado`. La transición se
valida contra el grafo de
[`transiciones_granularidad()`](https://sebollin.github.io/lupa/reference/granularidades.md).

## Usage

``` r
agregar(
  medidas,
  destino,
  funcion = c("ratio", "ratio_umbral", "promedio", "promedio_ponderado"),
  umbral = NULL,
  pesos = NULL,
  coleccion = NULL,
  colecciones = NULL,
  organizacion = NULL,
  organizaciones = NULL
)
```

## Arguments

- medidas:

  Data frame producido por
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) o una
  agregación anterior. Debe contener una sola métrica específica, una
  corrida y una granularidad.

- destino:

  Granularidad de destino.

- funcion:

  Una de `"ratio"`, `"ratio_umbral"`, `"promedio"` o
  `"promedio_ponderado"`.

- umbral:

  Umbral en `[0, 1]` requerido por `ratio_umbral`.

- pesos:

  Vector numérico requerido por `promedio_ponderado`, con una entrada
  por fila de `medidas`.

- coleccion:

  Frontera declarada, exigida cuando `destino` es `"coleccion"`: el
  objeto de
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
  o el perfil de
  [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md).
  Sin ella no se sabe sobre qué tablas se está agregando, y el número
  resultante no describiría nada.

- colecciones:

  Lista nombrada de objetos de
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
  o
  [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md),
  exigida cuando `destino` es `"conjuntoColecciones"`. Los nombres
  declaran la identidad y la frontera del conjunto; no se agregan
  organizaciones ni otros alcances implícitos.

- organizacion:

  Frontera institucional declarada con
  [`organizacion()`](https://sebollin.github.io/lupa/reference/organizacion.md),
  exigida cuando `destino` es `"organizacion"`. Qué bases pertenecen a
  un organismo **no está en los datos**, así que lo declara quien lo
  sabe.

- organizaciones:

  Lista de objetos de
  [`organizacion()`](https://sebollin.github.io/lupa/reference/organizacion.md),
  exigida cuando `destino` es `"conjuntoOrganizaciones"`.

  Los dos niveles institucionales son **opcionales**: un análisis de
  calidad no siempre tiene una organización detrás —una entrega suelta,
  un archivo que alguien mandó, una base sin dueño declarado—, y nada
  obliga a pasar por ellos. Existen para quien los necesita, y sin
  declaración `agregar()` se niega y explica cómo declararla, que es
  distinto de inventar una frontera que nadie nombró.

## Value

Objeto `medicion` agregado, con una fila por objeto de destino.

## Details

`ratio` sólo acepta medidas booleanas. `ratio_umbral` sólo acepta
medidas reales. Los promedios aceptan ambos tipos y siempre producen
resultado real en `[0, 1]`. Para el promedio ponderado, los pesos deben
estar en `[0, 1]` y sumar uno dentro de cada objeto de destino. La
columna `orientacion` se conserva sin invertir el resultado: un ratio de
una métrica de defecto sigue siendo la proporción de defectos.

No existe una transición hacia factor, dimensión o modelo: esos campos
son taxonómicos y esta función no calcula un índice global.

## Examples

``` r
nucleo <- metricas_nucleo()
especifica <- especializar(nucleo$NoNulo)
instancia <- instanciar(especifica, "personas", "edad")
medidas <- medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
agregar(medidas, "atributo", "ratio")
#>                                               id_medida
#> 1 medicion-20260824T235449.200122-7641-agg-ratio-000001
#>                            id_medicion               fecha metrica
#> 1 medicion-20260824T235449.200122-7641 2026-08-24 23:54:49  NoNulo
#>   metrica_especifica   metrica_instanciada   dimension   factor orientacion
#> 1             NoNulo agregada:ratio:NoNulo Completitud Densidad conformidad
#>   granularidad tipo_resultado  entidad atributo fila objeto_medible resultado
#> 1     atributo           real personas     edad   NA  personas$edad 0.6666667
#>   agregacion
#> 1      ratio
```
