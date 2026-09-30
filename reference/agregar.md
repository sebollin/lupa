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
  por fila de `medidas`. Si trae nombres, se emparejan con
  `objeto_medible` y se falla nombrando lo que sobra o falta —igual que
  en
  [`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)—,
  así que la misma declaración escrita en otro orden da el mismo número.
  Sin nombres se leen por posición.

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
medidas reales. Los promedios aceptan **esos dos tipos y ningún otro**,
y siempre producen resultado real en `[0, 1]`. Los tres tipos no
acotados —`numero_real`, `entero` y `duracion`— se rechazan nombrando la
métrica que los declara: el promedio de una duración en días es un
número de días, y publicarlo como resultado real en `[0, 1]` lo
presentaría como una proporción. La guarda es por **tipo declarado**, no
por el valor observado: antes una duración de `0,25` y `0,75` días
pasaba y la misma métrica con `1,5` días abortaba, así que el mismo
modelo cambiaba de conducta según los datos que le tocaran. Para el
promedio ponderado, los pesos deben estar en `[0, 1]` y sumar uno dentro
de cada objeto de destino. La columna `orientacion` se conserva sin
invertir el resultado: un ratio de una métrica de defecto sigue siendo
la proporción de defectos.

## Lo que el agregado conserva de la medición

Una agregación hereda lo que sus partes declararon:
`configuracion_modelo`, `configuracion_aplicabilidad`,
`cobertura_metricas`, `marco_calidad` y `fecha_declarada` viajan en el
objeto. Lo último no es decorativo: sin `fecha_declarada`, la guarda que
impide comparar dos entregas en orden invertido queda desactivada y
[`comparar_evaluaciones()`](https://sebollin.github.io/lupa/reference/comparar_evaluaciones.md)
publicaría el delta con el signo al revés.

`alcance_medidas` —«midió tres celdas de cuatro»— también viaja, pero
**reexpresado en la clave del agregado**: la tabla que publica
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) está
indexada por `metrica_instanciada` (`Formato@t.cod`) y el agregado
renombra la métrica (`agregada:ratio:Formato`), así que arrastrarla tal
cual dejaba una declaración que no se podía atribuir a ninguna fila. Los
conteos se **suman** por objeto de destino, y sólo cuando todas las
partes declaran la misma unidad; si no, la fila declara la mezcla en
lugar de publicar un total que no estaría en ninguna unidad.

Las coberturas de frontera (`cobertura_coleccion` y sus hermanas) **no**
se arrastran: cada agregación calcula la de su propio destino con las
partes que entraron a ese número.

## Los pesos se publican

Cuando hay `pesos`, el objeto los publica en `pesos_declarados`, con el
nombre de la parte que recibió cada uno, de modo que el número se pueda
rehacer con lo que el objeto trae. Y si alguna parte entró con peso
cero, se declara en `partes_con_peso_cero`: entró al número sin
aportarle nada, queda contada en la identidad de la fila y leerlo sin
saberlo es leer otra cosa. La declaración depende de que **haya** un
peso cero, no del destino: antes sólo existía en los cuatro destinos que
tienen cobertura de frontera, y en `conjuntoEntidades` el mismo peso
cero pasaba en silencio.

Cuando el destino es `conjuntoEntidades`, `promedio` combina las partes
sin pesos porque las medidas de nivel `entidad` no llevan su cantidad de
filas. El resultado lo declara en `advertencia_agregacion`: es un
promedio sin pesos y `agregar()` no conoce el alcance de cada parte. En
`coleccion`, la frontera se declara y por eso sólo se admite
`promedio_ponderado`: sin pesos no se puede decidir cuanto debe aportar
cada tabla de esa coleccion.

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
#> 1 medicion-20260930T004537.104234-7701-agg-ratio-000001
#>                            id_medicion               fecha metrica
#> 1 medicion-20260930T004537.104234-7701 2026-09-30 00:45:37  NoNulo
#>   metrica_especifica   metrica_instanciada   dimension   factor orientacion
#> 1             NoNulo agregada:ratio:NoNulo Completitud Densidad conformidad
#>   granularidad tipo_resultado  entidad atributo fila objeto_medible resultado
#> 1     atributo           real personas     edad   NA  personas$edad 0.6666667
#>   agregacion advertencia_agregacion
#> 1      ratio                   <NA>
```
