# Proponer un modelo de calidad desde el profiling

Construye el puente entre el examen de datos y la medición. El resultado
no ejecuta métricas: es una tabla editable donde cada fila conserva el
hallazgo o diagnóstico que la originó, la justificación, la
configuración y una marca `incluir`.
[`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md)
materializa únicamente las filas que el usuario deja activas.

## Usage

``` r
proponer_modelo(
  perfil,
  datos = NULL,
  relaciones = NULL,
  entidades_relacion = character(),
  max_valores_dominio = 20L,
  max_sugerencias = 100L
)
```

## Arguments

- perfil:

  Objeto creado por
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

- datos:

  Datos originales opcionales. Son necesarios para proponer dominios
  observados, materializar reglas de dependencia funcional e identificar
  los faltantes disfrazados que cuenta `NoNulo`.

- relaciones:

  Resultado opcional de
  [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md).

- entidades_relacion:

  Dos nombres de entidad correspondientes a las tablas usadas en
  `relaciones`.

- max_valores_dominio:

  Máximo de valores para proponer un dominio por extensión.

- max_sugerencias:

  Máximo de filas devueltas.

## Value

Data frame S3 de clase `propuesta_modelo`; las columnas de listas
contienen la configuración y los vínculos sin convertirlos en texto. Su
método [`print()`](https://rdrr.io/r/base/print.html) no falla por
codificación en ningún locale: la copia que se exhibe convierte lo que
trae codificación declarada, declara como UTF-8 los bytes válidos sin
declarar —R los escapa como `<U+00F1>` donde el locale no los
represente— y deja el resto tal cual mientras R pueda imprimirlo. Bajo
un locale UTF-8 la salida es exactamente la de R; bajo uno que no puede
representar un carácter difiere a propósito, publicando el punto de
código donde R escaparía los bytes. Sólo cuando R no puede imprimir se
reintenta con su propio octal, `\377`. Eso no convierte a
[`print()`](https://rdrr.io/r/base/print.html) en infalible: si una
clase suya tiene un método
[`format()`](https://rdrr.io/r/base/format.html) que da error, ese error
le llega sin alterar. Y un
[`format()`](https://rdrr.io/r/base/format.html) que consulte
[`Encoding()`](https://rdrr.io/r/base/Encoding.html) verá las marcas de
la copia, no las del objeto original.

## Details

Las reglas observadas que pueden sobreajustarse a una entrega —dominios
y patrones dominantes— se proponen inactivas. Los controles
estructurales directos, como `NoNulo`, duplicación exacta y dependencias
funcionales exactas, se activan. `max_sugerencias` recorta después de
ordenar por prioridad y el objeto declara el total en sus atributos.
Dentro de cada prioridad las filas se ordenan por métrica, entidad y
atributos según sus bytes —no según la intercalación del locale—: el
mismo orden en cualquier sesión.

`id_sugerencia` se arma con el **contenido** de la sugerencia, nunca con
su posición: la métrica y sus atributos en su orden —`NoNulo:edad`,
`ReglaIntegridadIntraEntidad:depto,codigo`— y, cuando la sugerencia liga
dos tablas, cada atributo con su entidad delante
(`ReglaIntegridadInterEntidad:personas.id,visitas.persona_id`). Una
sugerencia sin atributos lleva sólo la métrica (`EntidadDuplicada`). Así
una columna que se agrega o se quita no cambia el id de las demás, y una
regla de
[`regla_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md)
que lo nombra sigue midiendo lo mismo en la corrida siguiente:
[`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md)
lo usa como nombre de la instancia, que es la `metrica_instanciada` de
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md), del
tablero y del histórico. La entidad de una sugerencia de una sola tabla
no entra, porque es el nombre del perfil y cambia con la expresión
perfilada; la configuración tampoco: un cambio de patrón o de dominio lo
declara el histórico en `configuracion_modelo`. Dentro de cada nombre se
escribe `%25`, `%2C` y `%23` en lugar de `%`, `,` y `#` —y `%2E` en
lugar de `.` en una entidad—, y un nombre `NA` se escribe `%NA`, de modo
que dos contenidos distintos no dan el mismo id. Un nombre que no es
UTF-8 válido escribe cada byte alto como `%XX` —los bytes latin1 de
«notaé» sin declarar dan `nota%E9`—, así que el id es siempre texto
UTF-8 o ASCII, el mismo en cualquier locale. Si dos sugerencias tienen
el mismo contenido —una tabla que repite el nombre de una columna—, la
segunda lleva `#2` y la tercera `#3`, en el orden de las columnas de la
tabla.

Si una columna contiene faltantes disfrazados, `NoNulo` se propone
activa, con estado `lista`, y **se mide contándolos**: los valores que
el perfil contó como disfrazados —con la misma regla y la misma
declaración de `sentinelas_numericos` y `cadenas_ausencia`— se declaran
en `valores_nulos` de su `configuracion`, y la justificación lo dice.
Sobre cuarenta valores con un `NA` y tres `-999` mide 0,90, no 0,975. Si
no se pueden identificar —sin `datos`, la propuesta no tiene de dónde
sacarlos—, se mide igual con los valores crudos, y la justificación dice
que cuenta sólo los `NA` y que sobrestima la completitud.
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md) la
mide y
[`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md)
la materializa como a cualquier otra sugerencia activa. Si la columna se
declaró en `columnas_opcionales` de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
la `NoNulo` lleva además `opcional = TRUE` y se mide sobre las celdas
presentes, el mismo universo que el perfil: con un `NA` y tres `-999` en
cuarenta celdas mide 36/39 = 0,923, que es `1 - prop_faltantes_totales`.
Una sugerencia sobre una columna cuyo nombre la tabla repite sale
inactiva y con estado `nombre_repetido`: una métrica se liga por nombre,
y ese nombre no dice cuál de las columnas mide.
[`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md)
no la materializa y
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md) no
la mide hasta que las columnas se renombren.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md),
[`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md),
[`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)

## Examples

``` r
datos <- data.frame(
  codigo = rep(1:3, each = 4),
  categoria = rep(c("A", "B", "C"), each = 4),
  valor = c(1:11, NA)
)
propuesta <- proponer_modelo(perfilar(datos), datos)
propuesta[, c("metrica", "origen", "incluir")]
#>                       metrica                                  origen incluir
#> 1                      NoNulo              perfil:n_faltantes_totales    TRUE
#> 2 ReglaIntegridadIntraEntidad dependencia_funcional:categoria->codigo    TRUE
#> 3 ReglaIntegridadIntraEntidad dependencia_funcional:codigo->categoria    TRUE
#> 4                     Formato               perfil:patron_dominante:A   FALSE
#> 5 ValoresPosiblesPorExtension                perfil:dominio_observado   FALSE
```
