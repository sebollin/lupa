# Planificar el costo de `perfilar_dbi()` antes de pagarlo

Emite sólo consultas de preparación —leer el esquema y sondear
capacidades— y devuelve cuántas consultas emitiría el perfilado
completo, de qué clase y con qué alcance sobre la tabla. No escanea
datos para decidir el costo. Cuando
`politica_costo = "por_cardinalidad"`, una clave estructural exacta
puede cerrar la decisión; si no hay una fuente de catálogo utilizable,
el plan publica el rango entre omitir y ejecutar las métricas caras.
Nunca lanza `COUNT(DISTINCT ...)` para despejar esa incertidumbre. Las
fuentes estructurales se resuelven cuando la política necesita la
cardinalidad, aunque `estrategia_distintos` no permita medirla. La
disponibilidad de la estrategia gobierna la medición, no el conocimiento
que ya da el catálogo.

## Usage

``` r
plan_perfilado_dbi(
  conexion,
  tabla,
  muestra = Inf,
  orden_muestra = NULL,
  modo = c("exacto", "seguro", "conteos", "muestreado", "aproximado"),
  metricas = NULL,
  max_consultas = Inf,
  dialecto = "auto",
  incluir_valores = TRUE,
  tamano_lote = NULL,
  tamano_lote_planos = .TAMANO_LOTE_PLANOS_DBI,
  tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
  bloque_muestra = c("con_muestra", "solo_agregados"),
  instrumentar = FALSE,
  estrategia_distintos = "exacta",
  politica_costo = c("todas", "ninguna", "por_cardinalidad", "cardinalidad"),
  umbral_cardinalidad = .UMBRAL_CARDINALIDAD_COSTO_DBI
)
```

## Arguments

- conexion:

  Conexión abierta compatible con DBI.

- tabla:

  Nombre de tabla o un objeto aceptado por
  [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html).

- muestra:

  Cantidad positiva de filas solicitadas para el perfil de muestra, o
  `Inf` para traer la tabla entera. Con `Inf` la consulta sale sin
  `LIMIT` y `tabla_completa` queda en `TRUE`.

  El resumen de tabla **no** se muestrea: con `modo = "exacto"` se
  calcula en el motor sobre todas las filas. Lo que sale de esta muestra
  son los diagnosticos que necesitan los valores en R -patrones,
  formatos, casi-duplicados-, y sin `orden_muestra` no son una muestra
  aleatoria sino las primeras filas que devuelva el motor. Medido sobre
  una tabla de 200.000 filas con un defecto plantado al final: con el
  valor por omision aparecen tres hallazgos y con `Inf` aparecen cinco,
  a cambio de 10 segundos en vez de 2. Un analisis de calidad no se
  corre todos los dias; si el tiempo no es la restriccion, `Inf` es la
  opcion honesta.

- orden_muestra:

  Columnas para `ORDER BY`. La salida solo declara orden reproducible
  cuando la combinación es única en toda la tabla. Sin este argumento,
  DBI no garantiza el orden ni la pertenencia de una muestra limitada, y
  `meta` lo declara expresamente. No se usa cuando
  `bloque_muestra = "solo_agregados"`.

- modo:

  Conjunto de métricas del resumen: `"exacto"` las calcula todas,
  `"seguro"` evita las que ordenan o agrupan la tabla completa y
  `"conteos"` deja solo el conteo de valores no nulos, `"muestreado"`
  calcula estimaciones sobre filas elegidas por el motor y
  `"aproximado"` usa funciones nativas aproximadas para las métricas que
  ese modo define.

- metricas:

  Selección explícita de grupos de métricas, que tiene prioridad sobre
  `modo`: `"validos"`, `"distintos"`, `"moda"`, `"basicos"`, `"mediana"`
  y `"desvio"`.

- max_consultas:

  Presupuesto declarado de consultas. Al agotarse, las métricas
  restantes quedan en `no_disponible` con ese motivo.

- dialecto:

  Capacidad de acotar filas: `"auto"` la sondea, y `"limit"`, `"top"`,
  `"fetch_first"`, `"rownum"` o `"portable"` la declaran sin sondeo.

- incluir_valores:

  Si el resumen informa valores de celda: moda, mínimo, máximo y
  mediana. Con `FALSE` esas consultas no se emiten.

- tamano_lote:

  Cantidad máxima de columnas por consulta consolidada. Se conserva por
  compatibilidad y, si se informa, fija el tamaño de las dos familias.
  Para control separado, usar `tamano_lote_planos` y
  `tamano_lote_distintos`.

- tamano_lote_planos:

  Cantidad máxima de columnas por consulta de agregados planos. El valor
  por omisión es 20.

- tamano_lote_distintos:

  Cantidad máxima de columnas por consulta de cardinalidades exactas. El
  valor por omisión es 2, medido sobre el servidor de referencia: el
  `Shared Read` fue constante entre lotes y el costo por columna fue
  casi igual para uno y dos, mientras el lote de dos derramó menos que
  los lotes mayores. Una sola cardinalidad todavía puede forzar un
  agregado pesado y derramar mucho más que un lote plano.

- bloque_muestra:

  Qué bloques se solicitan: `"con_muestra"` (por omisión) calcula
  también `perfil_muestra`, o `"solo_agregados"` omite su lectura y
  devuelve sólo los agregados SQL. La segunda opción no cambia el
  alcance de esos agregados: eso lo decide `modo`.

- instrumentar:

  En el plan, si es `TRUE`, cronometra las consultas de preparación. No
  habilita consultas de datos ni agrega mediciones al objeto devuelto:
  sus costos siguen siendo predicciones. Por omisión es `FALSE`.

- estrategia_distintos:

  Procedencia explícita para `n_distintos`: `"exacta"` (por omisión)
  emite `COUNT(DISTINCT)`; `"aproximada_motor"` usa una función nativa
  aceptada por el motor y deja la métrica en `no_disponible` si no
  existe; `"catalogo"` queda declarada pero `no_disponible` hasta
  implementar la estadística del catálogo; y `"omitida"` no emite
  ninguna consulta. No hay repliegue automático entre estrategias. El
  resultado publica `estrategia_solicitada`, `estrategia_resuelta` y
  `estado` en `meta$estrategia_distintos`, y las dos primeras también en
  `resumen_tabla$sql`.

- politica_costo:

  Política optativa para las métricas caras. El valor por omisión,
  `"todas"`, conserva moda y mediana para todas las columnas
  solicitadas. `"ninguna"` es un alias de `"todas"`;
  `"por_cardinalidad"` (también `"cardinalidad"`) resuelve primero las
  fuentes estructurales y mide valores válidos y distintos sólo cuando
  hace falta y la estrategia lo permite. Luego omite, por columna, moda
  y mediana cuando la proporción de distintos alcanza
  `umbral_cardinalidad`. Una estrategia no disponible no se convierte en
  una medición exacta.

- umbral_cardinalidad:

  Proporción entre valores distintos y válidos que activa
  `politica_costo = "por_cardinalidad"`. El valor por omisión es `0.95`
  sólo cuando esa política se pide explícitamente; se puede mover en
  cada llamada. Para pedir todas las métricas use
  `politica_costo = "todas"`.

## Value

Data frame de clase `plan_perfilado_dbi` con `clase_consulta`,
`n_consultas`, `n_consultas_max` y `alcance`, y los atributos `total`,
`total_minimo`, `total_maximo`, `total_lotes_rechazados`, `columnas`,
`columnas_numericas`, `dialecto`, `consultas_emitidas`, `metricas`,
`metricas_ejecucion`, `politica_costo`, `estrategia_distintos`,
`fuente_cardinalidad_costo`, `mediana_consolidada`, `filas`,
`tamano_lote_planos` y `tamano_lote_distintos`. Cuando se pide
`bloque_muestra = "solo_agregados"`, también conserva ese valor en el
atributo `bloque_muestra` y no incluye la fila de la lectura de muestra.

El costo no se declara como un número sino como un rango: `total` es el
extremo inferior, que supone que la política omite las métricas caras
cuya cardinalidad no se conoce, y `total_maximo` el superior, que supone
que las ejecuta. Ambos incluyen la preparación y el perfilado previsto;
el rechazo de lotes puede agregar las sondas de bisección declaradas por
`total_lotes_rechazados`. El costo real cae entre los extremos, y
`attr(plan, "supuesto")` dice por qué se mueve en cada dirección.

Cuántas consultas se emiten no dice cuánto cuestan: catorce consultas
sobre dos millones de filas son mucho más trabajo que doscientas sobre
mil. Por eso el plan estima además la magnitud, y la estima en sus dos
mitades, porque el reloj de una corrida no lo decide siempre el motor.

La del motor va en `filas_leidas` (cuántas filas habría que leer) y
`ordenaciones_completas` (cuántas veces habría que ordenar la tabla
entera), y se resume en `magnitud_motor`. La del cliente va en
`columnas_texto` y `pares_texto` —cuántos pares de formas podría
comparar en R el detector de vocabulario sobre la muestra— y se resume
en `magnitud_texto`. `magnitud` es la mayor de las dos: `"baja"`,
`"media"`, `"alta"`, o `"desconocida"` si no se conoce el número de
filas. `supuesto_costo` dice de dónde sale cada cuenta.

El plan previo no publica duraciones, CPU, filas ni bytes medidos. El
plan de una corrida de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
puede agregar `costo_distintos` cuando ya hay duraciones de agregados
planos de esa misma corrida; sus campos dicen explícitamente que la
proyección sigue siendo una estimación.

Si se pide `politica_costo = "por_cardinalidad"`, el plan busca primero
una garantía estructural o una fuente de catálogo. Si la fuente queda
desconocida, no emite un agregado para aclararla: `n_consultas` omite
moda y mediana, y `n_consultas_max` deja abierto el camino que las
ejecuta. La corrida mide `distintos` sólo si la política lo necesita. La
política por omisión es `"todas"`: el paquete no elige por el usuario.
Una fuente estructural se resuelve aunque la estrategia de distintos
este omitida o no disponible; esta ultima solo gobierna si se puede
medir.

Contar sólo el motor daba juicios falsos con números ciertos: una tabla
de 3.912 filas con una columna de geometría en texto pedía 64.592
lecturas —magnitud `"baja"`— y tardaba 35 segundos, porque el trabajo
estaba en la comparación de formas, que no es una lectura de fila. El
método de impresión muestra las dos mitades, avisa cuando la magnitud es
alta y nombra las palancas para acotarla, que no son las mismas de un
lado que del otro.

## Details

`estrategia_distintos` declara la procedencia de `n_distintos` antes de
la corrida y conserva por separado lo pedido, lo resuelto y el estado.
No hay `auto`: `"exacta"` es el valor por omisión, `"aproximada_motor"`
queda `no_disponible` si el motor no ofrece una función aceptada,
`"catalogo"` queda `no_disponible` hasta implementar su estadística y
`"omitida"` no emite el agregado. `fuente_cardinalidad_costo` sigue
siendo independiente y sólo describe el número usado por la política de
costo cuando esa política se pide.

El plan previo no puede publicar segundos medidos porque no lee los
datos. Durante
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
en cambio, los agregados planos se ejecutan antes que los distintos. Si
se midieron en esta misma corrida, el plan que queda en
`resumen_tabla$meta$plan` agrega `costo_distintos`: la mediana de esas
duraciones multiplicada por la cantidad de lotes de distintos. Es una
estimación rotulada, fundada en la tabla y el servidor actuales, no en
`reltuples` ni en otra estadística de catálogo. El aviso se emite antes
de iniciar el primer `COUNT(DISTINCT)` y sólo si supera el umbral de 30
segundos; no pide confirmación y nunca bloquea un guion no interactivo.

## See also

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)

## Examples

``` r
if (requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "ejemplo", data.frame(id = 1:10, valor = 11:20))
  plan_perfilado_dbi(con, "ejemplo")
  DBI::dbDisconnect(con)
}
```
