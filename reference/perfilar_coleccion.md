# Perfilar una colección declarada

Recorre las tablas declaradas en
[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
y devuelve **una fila por tabla** con su resumen exacto, más la
cobertura de lo que no se pudo medir o no se solicitó.

## Usage

``` r
perfilar_coleccion(
  coleccion,
  muestra = Inf,
  conservar_perfiles = FALSE,
  cobertura_metricas = c("no_medidas", "completa", "ninguna"),
  tope_cobertura_metricas = 20000L,
  bloque_muestra = c("con_muestra", "solo_agregados"),
  ...
)
```

## Arguments

- coleccion:

  Objeto creado por
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md).
  Una colección **guardada por una versión anterior** se perfila igual.
  El objeto fue creciendo —`catalogo`, `tipo`, `declaracion`— y las
  columnas que a un objeto viejo le faltan se completan con el valor que
  significa «esta colección no lo declaró», mientras `identificador` y
  `referencia`, que son derivados, se recalculan con las mismas
  funciones que los arman. Lo único que no se puede inventar es `tabla`,
  que es el nombre mismo: su ausencia se rechaza nombrando la columna.
  Antes faltaba cualquiera de seis columnas y la corrida entera moría
  con un error de
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) que no
  nombraba ni la columna ni la tabla —o, sin `referencia`, perfilaba
  **cero** tablas sin abortar—, y eso contradice la decisión de que ante
  un fallo parcial se devuelve lo medido con su alcance declarado.

- muestra:

  Filas solicitadas por tabla para el bloque en memoria. Por omision
  `Inf`: cada tabla se trae entera. El resumen por tabla se calcula en
  el motor y no depende de esto; lo que depende son los diagnosticos que
  miran los valores en R. Acotarlo con `muestra = n` es mas rapido a
  cambio de mirar menos filas, y sobre una coleccion grande conviene
  mirar antes
  [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
  tabla por tabla.

- conservar_perfiles:

  Si se retienen los objetos `perfil_dbi` completos. Por omisión
  `FALSE`. Retenidos y con la protección activa, cada uno se protege
  además con los valores protegidos de las **demás** tablas de la
  colección: una observación de `movimientos` que cita la cédula y el
  nombre de un cliente se tapa aunque `movimientos` no tenga ninguna
  columna personal, porque la colección es un solo objeto. Esos valores
  se leen de la base, una consulta por columna protegida, con la regla
  de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  —también la de los rellenos, con sus frecuencias—; si alguna lectura
  falla —también si una tabla entera no se pudo perfilar, por ejemplo
  porque el rol no tiene permiso sobre ella, o no se perfiló porque se
  declaró de otro tipo —`tipo = "vista"`—: sus valores son
  desconocidos—, o una tabla no se pudo clasificar porque su muestra no
  se leyó, en los perfiles de las otras tablas se tapan los valores y la
  evidencia de las columnas no personales, y
  `resumen_tabla$meta$proteccion_personal$otras_tablas` dice por qué. Y
  con esa regla, una columna que copia SIN su verificador los documentos
  protegidos de OTRA tabla —`tramites.ref` con la cédula de `personas`
  sin el dígito— se protege entera, como la que los copia en su propia
  tabla, y su fundamento dice de qué tabla salen: sus valores distintos
  se leen de la base, una consulta por columna no protegida que publica
  un número de seis cifras o más, que la muestra clasificó como
  documento o cuyos valores la muestra describe como cifras con
  separadores —`"4.685.467,00"`, `"4 685 467.0"`, `"4.685467e6"`, aunque
  los acompañe un `"S/D"`—, con la misma expresión con que la leyó
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  —`money` y `real` convertidos—; la prueba es sobre la tabla entera y
  en cualquier orden de las tablas, y la tabla con una copia se vuelve a
  perfilar con esa columna como personal; si la lectura o el nuevo
  perfil fallan, el perfil de esa tabla se cierra igual. Limitación
  conocida (baja): ese nuevo perfil reemplaza al conservado, pero
  `resumen_coleccion`, `cobertura_coleccion`, `cobertura_metricas` y
  `meta$lecturas` quedan con lo del primero —los estados de las métricas
  de la copia dicen `calculado` donde el perfil dice que se ocultaron—;
  no publican sus valores. Con esos valores se barren también
  `cobertura_coleccion`, `cobertura_metricas` y `meta$lecturas`, que
  juntan los motivos de todas las tablas. Con o sin retenerlos, esos
  tres campos, los perfiles retenidos y el mensaje del motor de cada
  tabla pasan además por el piso con los valores que las tablas leyeron
  y que un mensaje del motor puede llevar —todo valor con letras, porque
  cualquier palabra puede ser el nombre de una columna de otra tabla, y
  el número que el mensaje no omite entero, como `"099;123;456"`,
  escrito con un signo que no une; ver
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)—,
  sin consultas de más: el mensaje de una vista que citaba
  `"Campos Cadena"`, la celda de su tabla base, se publicaba; y con
  perfiles retenidos y una tabla base incompleta —una columna protegida
  `ntext` de SQL Server, que no admite el `GROUP BY` que lee sus
  valores— el piso usaba sólo las tablas completas y el motivo de la
  vista citaba el valor entero. De cada uno se guarda una huella —ocho
  bytes de su forma plegada y sin separadores— y no el texto: la memoria
  no crece con el largo de los nombres que se leen, y un tramo del
  mensaje con la huella de uno se tapa como esté escrito; dos formas con
  la misma huella, una en 2^52, sólo pueden tapar de más. Y cada mensaje
  del motor se guarda como llegó mientras dura la colección —en memoria;
  nunca entra al objeto— y al final se vuelve a leer con todo lo que la
  colección leyó: con una vista perfilada antes que su tabla base el
  piso no conocía todavía el nombre, y la segunda pasada dejaba su resto
  —`"De Los Campos"` de `"María De Los Campos"` en SQL Server en sueco,
  `"Campos Cadena"` después de un apodo entre comillas—. El orden de las
  tablas no cambia lo que se publica; lo vuelto a leer reemplaza a lo
  publicado sólo si publica menos. La relectura corre también después
  del cruce entre tablas, sobre lo que el cruce escribe: el motivo de
  `otras_tablas` lleva el mensaje del motor de la tabla que no se pudo
  leer, y con la vista antes que su base citaba ese resto. En ese
  cierre, un valor igual al nombre de una tabla o de un esquema de la
  colección no se tapa: es estructura, como en
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
  Se reemplaza el texto tal como se publicó: si el piso de la propia
  tabla ya lo había cambiado —el mensaje citaba entero, además, un valor
  protegido de esa misma tabla—, la relectura no lo encuentra y queda
  sólo el piso de las huellas, que tapa los valores enteros y no el
  resto. Y los avisos de consola de cada tabla se emiten al final,
  después del mismo piso que esos tres campos, con o sin perfiles: el
  aviso de una vista perfilada antes que su tabla base citaba la celda.
  También si la colección se corta —una interrupción, un error— después
  de leer alguna tabla: los avisos juntados salen por ese mismo piso,
  con los valores que se conocían.
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  sobre una tabla sola protege sólo con los de esa tabla. Sin protección
  (`proteger_datos_personales = FALSE`) no se cruza nada: cada perfil
  retenido sale como lo dio su tabla, también si otra no se pudo
  perfilar.

- cobertura_metricas:

  Qué se sube a `cobertura_metricas`: `"no_medidas"` (por omisión) sólo
  las métricas que no se calcularon, `"completa"` todas con su estado,
  `"ninguna"` para omitirla. Los conteos por tabla de
  `resumen_coleccion` no dependen de esta elección.

- tope_cobertura_metricas:

  Máximo de filas de `cobertura_metricas`. El total real queda siempre
  en `meta$n_metricas_no_medidas`, aunque la tabla se haya recortado.

- bloque_muestra:

  Qué bloques se solicitan en cada tabla: `"con_muestra"` (por omisión)
  calcula también el perfil de la muestra, o `"solo_agregados"` evita
  leerla y devuelve sólo los agregados SQL. Se pasa al mismo argumento
  de
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  para que todas las tablas de la colección respeten la misma decisión.

- ...:

  Argumentos enviados a
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
  `proteger_datos_personales` vale para toda la colección: sin
  protección, el mensaje de un error del motor se publica entero —el
  literal que hizo fallar la lectura y el SQL— también en
  `cobertura_coleccion`, `cobertura_metricas`, `meta$lecturas` y los
  avisos; ver la sección «Datos personales» de
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
  Lo que va a
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  se valida al entrar, una vez. Los argumentos propios de
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  —`estrategia_distintos`, `max_consultas`, `umbral_cardinalidad` y los
  demás— los valida
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  en cada tabla: uno mal escrito deja cada tabla en
  `cobertura_coleccion` con ese motivo y sin aviso. Es una limitación
  conocida: si ninguna tabla se perfiló, mirar el motivo antes de
  sospechar de los permisos.

## Value

Objeto de clase `perfil_coleccion` con `resumen_coleccion`,
`cobertura_coleccion`, `cobertura_metricas`, `meta` y, si se pidió,
`perfiles`.

## Details

Conserva el resumen calculado en SQL sobre toda la tabla, que es
acotado, y **no** retiene el perfil de la muestra de cada tabla, que es
el objeto pesado: con cientos de tablas eso no entra en memoria.
`conservar_perfiles` permite retenerlos cuando la colección es chica y
se los necesita.

Cada fila de `cobertura_coleccion` nombra la tabla con las tres partes
de su identidad —`catalogo`, `esquema` y `tabla`—, las mismas que
[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
declaró. Es lo que permite que el motivo siga pegado a la tabla cuando
el número sube de nivel: sin el catálogo, una tabla declarada como
`catalogo.esquema.tabla` se publicaba río abajo como `esquema.tabla`.

**Lo que no se pudo medir se declara, y nunca a cero.** Una tabla que la
credencial no puede leer, un objeto que no es una tabla base y una tabla
vacía van a `cobertura_coleccion` con su motivo y su `como_resolverlo`.
Un motor que rechaza un agregado —o una columna cuyo tipo no lo admite—
va a `cobertura_metricas`, con una fila por columna y métrica, y deja
además una línea de resumen en `cobertura_coleccion`. Esa cobertura por
métrica **se calcula antes de descartar el perfil**, así que existe
también con `conservar_perfiles = FALSE`. En bases institucionales los
permisos parciales son el caso normal, no el borde.

**Donde no hubo medición va `NA`, no `0`.** Una tabla vacía deja
`prop_faltantes` en `NA` en todas sus columnas: antes eso producía
`prop_faltantes_maxima = -Inf` —fuera del `[0, 1]` que el paquete
promete, y ordenada como la tabla de mejor calidad de la base— y
`n_columnas_sin_faltantes = 0`, que afirmaba tres columnas sin ausencias
sobre una tabla de la que no se sabía nada. `n_columnas_medidas` declara
sobre cuántas columnas se conoce la proporción de ausentes, para que los
dos números anteriores se lean con su alcance.

**No hay lectura instantánea.** Perfilar una colección son muchas
consultas y la base puede cambiar entre ellas: una tabla puede truncarse
después de que se contaron sus filas. Por eso cada fila declara el
`momento` en que se midió y `meta$snapshot` declara que no lo hubo.

Cada fila de `resumen_coleccion` trae `universo`, que nombra el universo
de las metricas SQL que alimentan sus agregados. En particular, con
`universo = "muestra_motor"`, `n_filas` sigue siendo el conteo de la
tabla completa pero las proporciones observadas declaran que provienen
de la muestra, en vez de presentarse como cifras de la tabla entera.

`meta$lecturas` conserva el SQL emitido por las tablas perfiladas, con
su identidad, aunque `conservar_perfiles = FALSE`.
[`sql_perfil()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
lo devuelve como una tabla unica; los perfiles completos siguen
disponibles por tabla cuando se pidio conservarlos.

**Limitación conocida: las declaraciones por columna valen para todas
las tablas.** `columnas_codigo`, `columnas_opcionales`,
`columnas_personales` y las demás llegan por `...` iguales a cada
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
y
[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
no tiene declaraciones por tabla. Una tabla que no tiene una columna
declarada en `columnas_codigo` no se perfila —sale de
`resumen_coleccion` y `cobertura_coleccion` nombra la causa,
«`columnas_codigo` nombra columnas inexistentes», aunque
`como_resolverlo` hable de permisos—; con `columnas_opcionales` o
`columnas_personales` la tabla conserva su resumen y pierde la muestra.
`columnas_sin_ceros` y `columnas_no_negativas` no la afectan. Para
declarar columnas que no están en todas las tablas, perfilar esas tablas
por separado o partir la colección.

## See also

[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md),
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)

## Examples

``` r
if (requireNamespace("RSQLite", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:3, nombre = letters[1:3]))
  perfilar_coleccion(coleccion(con, "personas", nombre = "padron"))
  DBI::dbDisconnect(con)
}
```
