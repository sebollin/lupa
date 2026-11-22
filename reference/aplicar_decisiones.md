# Aplicar las decisiones guardadas a una corrida

Cruza las decisiones de corridas anteriores con los hallazgos de esta.
Cada hallazgo queda en uno de estos estados, en la columna
`estado_decision` de
[`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
y de
[`identificar_hallazgos()`](https://sebollin.github.io/lupa/reference/identificar_hallazgos.md):

## Usage

``` r
aplicar_decisiones(x, decisiones, datos = NULL, nombre_tabla = NULL)
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

- decisiones:

  Tabla de
  [`decidir_hallazgos()`](https://sebollin.github.io/lupa/reference/decidir_hallazgos.md)
  o ruta de un CSV de
  [`guardar_decisiones()`](https://sebollin.github.io/lupa/reference/guardar_decisiones.md).

- datos:

  La tabla que se perfiló, para comprobar la huella de los valores
  afectados. Si las decisiones se tomaron con ella y no se pasa, el
  hallazgo vuelve a `revisar`.

- nombre_tabla:

  Nombre de la tabla que se perfiló; obligatorio con el resultado de
  [`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md).

## Value

`x`, con las decisiones adjuntas (en el campo `decisiones_hallazgos`; en
el resultado de
[`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md),
en el atributo del mismo nombre y con las columnas de estado agregadas a
la tabla).

## Details

- `"pendiente"`: sin decisión.

- `"aceptado"`, `"descartado_falso_positivo"`,
  `"descartado_propiedad_legitima"`: decidido, con su motivo, quién y
  cuándo (`motivo_decision`, `decidido_por`, `fecha_decision`). **No
  desaparece**: sigue en la tabla, pero deja de contarse como pendiente.

- `"decidido_sin_verificar"`: hay una decisión pero **no se pudo
  verificar** que el hallazgo sea el mismo, porque no hay con qué
  comparar su contenido (la decisión se tomó sin `datos` ni clave, o el
  archivo no trae las huellas, o la trazabilidad está truncada). Se
  muestra con su motivo y lo que faltó en `verificacion`, y se cuenta
  **aparte** de lo decidido y verificado: la decisión no se aplica en
  silencio. Pasar `datos` o una `clave` al decidir y al aplicar la
  convierte en una decisión verificada.

- `"revisar"`: hay una decisión, pero el hallazgo **cambió** desde que
  se decidió —otra severidad, otra cantidad de filas afectadas, otros
  valores o otras filas afectadas (ver la huella en
  [`decidir_hallazgos()`](https://sebollin.github.io/lupa/reference/decidir_hallazgos.md))—
  y la decisión no se aplica en silencio; `cambio_desde_decision` dice
  qué cambió. También es `revisar` si al decidir había huella y ahora no
  se puede calcular (por ejemplo, se aplica sin `datos`): no se puede
  confirmar que sea el mismo. Con otra muestra de la misma tabla las
  cifras cambian y el hallazgo vuelve a `revisar`: es lo honesto. Un
  hallazgo sobre una columna de nombre repetido sólo se distingue por su
  contenido: sin huella que comparar, vuelve a `revisar`.

El estado del plan se **deriva** de las decisiones, no se acumula. La
capa de decisiones registra qué filas del plan tocó y qué valor tenían
antes; al reaplicar devuelve sólo esas filas a ese valor y aplica las
decisiones vigentes, así que aplicar dos veces, o con decisiones
cambiadas, da lo mismo que aplicar una vez las últimas, y las ediciones
a mano de las demás filas del plan se conservan.
[`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
sobre un perfil con decisiones aplicadas las respeta igual. Convertir el
tipo de una columna (`convertir_tipo`) quita sus espacios aunque se haya
descartado `espacios_sobrantes`: es inherente a la conversión, y la nota
de esa acción lo dice.

Sobre un
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
las acciones del plan de limpieza de un hallazgo **descartado** (falso
positivo o propiedad legítima) quedan con `aplicar = FALSE` y una nota
en su justificación que nombra la decisión, para que
[`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
por omisión no limpie lo que se dijo que es legítimo. Un descarte **sin
verificar** también deja sus acciones sin aplicar, con la nota «revisala
antes de limpiar»: nunca se modifica un dato sobre una decisión que no
se confirmó. Un `revisar` no aplica la decisión: el plan queda como
estaba, con una nota que nombra la decisión anterior y qué cambió. Las
acciones de un hallazgo aceptado no cambian. Sobre una colección que no
conservó los perfiles de sus tablas no hay hallazgos por tabla: se avisa
y se devuelve la colección tal cual.

Los hallazgos de la tabla entera de
[`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md)
(`hallazgos_tabla`, el atributo del resultado) no se pueden decidir: las
decisiones valen sobre las filas de la tabla de hallazgos por grupo.

Las decisiones que ya no encuentran su hallazgo (el defecto se corrigió,
o la tabla cambió de nombre) no se pierden: se cuentan en la impresión.

[`print()`](https://rdrr.io/r/base/print.html) muestra los conteos de
error y sospechoso sólo con lo pendiente (lo pendiente y lo que hay que
revisar de nuevo), y aparte lo decidido;
[`summary()`](https://rdrr.io/r/base/summary.html) de un perfil trae el
cruce de estado por severidad en el atributo `decisiones`;
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
agrega una sección con lo decidido y su motivo. El perfil sin decisiones
aplicadas es idéntico al de siempre.

Las huellas distinguen el tipo y el largo de cada valor (`NA` de `"NA"`,
un número de su texto, una fecha de su texto, `0.3` de `0.1 + 0.2`).

La identidad de un hallazgo incluye el nombre de la tabla (`nombre` en
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md);
el nombre de la tabla en
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
y las colecciones): con otro nombre, las decisiones no se aplican y los
hallazgos vuelven como pendientes. Dos tablas a las que se da el mismo
nombre son, por declaración, la misma: las decisiones de una valen en la
otra. Por eso un nombre que no identifica —`datos`, el que se usa cuando
no hay uno, o el que dejó un perfil de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
guardado por una versión anterior con un
[`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html)— se rechaza y se
pide `nombre_tabla`.

## See also

[`decidir_hallazgos()`](https://sebollin.github.io/lupa/reference/decidir_hallazgos.md),
[`guardar_decisiones()`](https://sebollin.github.io/lupa/reference/guardar_decisiones.md),
[`identificar_hallazgos()`](https://sebollin.github.io/lupa/reference/identificar_hallazgos.md)

## Examples

``` r
perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
decisiones <- decidir_hallazgos(
  perfil, "falso_positivo", motivo = "Revisado en la fuente.",
  quien = "equipo de datos", tipo_hallazgo = "faltantes"
)
nuevo <- aplicar_decisiones(perfil, decisiones)
table(hallazgos(nuevo)$estado_decision)
#> 
#> decidido_sin_verificar              pendiente 
#>                      1                     20 
```
