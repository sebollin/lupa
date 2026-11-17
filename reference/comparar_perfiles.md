# Comparar dos perfiles y detectar deriva estructural

Compara entregas sin exigir que tengan las mismas columnas. Devuelve
cambios de esquema, tipos, faltantes, cardinalidad, rango, patrones y
hallazgos como un objeto de datos filtrable.

## Usage

``` r
comparar_perfiles(anterior, actual, umbral_cambio = 0.05, umbral_error = 0.2)
```

## Arguments

- anterior, actual:

  Objetos producidos por
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  o
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md).
  De un análisis se compara su perfil, `$perfil`: sus mediciones y su
  evaluación tienen su propia serie en
  [`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md).
  Un objeto de
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  tiene que traer su perfil de muestra
  (`bloque_muestra = "con_muestra"`, el valor por omisión): sin él no
  hay tipos de columna ni nada medido sobre las filas, y se rechaza.

- umbral_cambio:

  Diferencia mínima para considerar significativo un cambio de
  proporción, cardinalidad o rango relativo. Cinco puntos porcentuales
  evita elevar variaciones pequeñas a hallazgo.

- umbral_error:

  Diferencia a partir de la cual un aumento de faltantes o un cambio de
  patrón —uno que aparece, desaparece o cambia de proporción— se
  clasifica como `error`.

## Value

Data frame `deriva_perfil`. `severidad` usa el factor ordenado
`ok < sospechoso < error`; los cambios menores permanecen como filas
`ok` para que la serie sea exportable sin ocultar diferencias. El
atributo `campos_protegidos` declara lo que no se comparó porque la
protección de datos personales lo tapa: un data frame con `columna`,
`campo` y `lado` —`anterior` o `actual`—, la misma forma que usa
[`comparar_equivalencia()`](https://sebollin.github.io/lupa/reference/comparar_equivalencia.md).
Los campos que pueden figurar son `rango` y `vocabulario`. Está siempre,
vacío si no hay nada que declarar, y
[`rbind()`](https://rdrr.io/r/base/cbind.html) une los de todas las
derivas.

`universo_anterior` y `universo_actual` dicen de qué filas sale la cifra
de cada lado: `"tabla_completa"` —un perfil en memoria, una muestra que
cubre la tabla o un agregado SQL calculado sobre la tabla entera—,
`"muestra"` —sólo las filas leídas, o los patrones y el tipo inferido
que
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
buscó en una `muestra` menor que la tabla— o `"mixto"` —los faltantes de
un objeto de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
cuya muestra no cubre la tabla: los `NULL` que cuenta el motor más los
faltantes disfrazados estimados sobre la muestra—. Las columnas
presentes son las del esquema. El conteo de filas es de la tabla entera:
el total de la fuente o el de un perfil que la cubre; sin ninguno de los
dos —el presupuesto de consultas no alcanzó para el `COUNT(*)`— la fila
`filas` sale `no_comparable`, porque lo que queda es el tamaño de la
muestra. El tipo declarado y la unidad salen de la clase con que R leyó
las filas: de un perfil que no cubre la tabla se rotulan `muestra`,
salvo que el tipo que declara el motor respalde el cambio. Son `NA` en
las filas de configuración, en la fila `muestra` y en las que declaran
algo que no se comparó. Un hallazgo se rotula con las filas del perfil;
el diagnóstico que trabaja sobre una submuestra lo declara en
`cobertura_diagnosticos`. [`rbind()`](https://rdrr.io/r/base/cbind.html)
une una deriva guardada antes de que existieran estas columnas con ellas
en `NA`.

## Details

Los patrones se comparan sobre el resumen acotado que conserva cada
perfil, no sobre los valores originales ni una distribución completa. Si
las dos corridas usaron configuraciones de patrones diferentes, se
informa un error de comparabilidad y esa parte de la comparación se
omite. Dos perfiles que buscaron sus patrones en todas las filas de la
tabla tienen la misma configuración aunque declaren otra `muestra`: con
400 filas, el `1e5` de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
el `Inf` que
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
le pasa a su muestra miden lo mismo. La fila publica lo declarado.

Un perfil DBI con `meta$origen_dbi$muestreo` o con
`meta$alcance$universo_id = "muestra_motor"` describe una muestra de la
tabla fuente. Si uno de los perfiles es de ese tipo, las filas se
comparan con `filas_totales_fuente` cuando está disponible, pero rangos,
cardinalidades, patrones y hallazgos de la muestra no se interpretan
como cambios de la tabla completa. La proporción de faltantes sí se
estima sobre las filas observadas y se compara, con
`universo = "muestra"`. La salida agrega una fila
`muestra / no_comparable`, cuya evidencia dice cómo se leyó cada lado
—el método y si la lectura es reproducible—, y deja fuera esos aspectos;
una muestra distinta no se convierte en un cambio ni en un hallazgo
resuelto. Con el objeto entero de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
esos aspectos se toman del resumen SQL: ver la sección siguiente.

Los nombres de columnas y las configuraciones que contienen texto del
usuario se comparan por sus bytes, no por la marca de codificación ni
por la intercalación del locale.

Eso alcanza mientras los bytes lleguen intactos, y hay un caso en que no
llegan y **no depende de este paquete**: el formato RDS 3 —el de
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) por omisión— anota la
codificación nativa de quien escribe, y al leer *traduce* desde ella el
texto que no declara la suya. Un perfil guardado bajo `LC_CTYPE=C` y
releído bajo otro locale puede volver con el texto cambiado; en Windows
el cambio es silencioso, porque ahí la conversión no falla.

Cuando eso pasa, el perfil releído **está** alterado, y conviene saber
qué alcanza a ver esta función. Compara las **propiedades medidas** de
cada columna, los hallazgos, el alcance del resumen y las
configuraciones declaradas —no los valores uno por uno—: la columna
`aspecto` del resultado nombra, fila por fila, qué se comparó, y es la
enumeración viva. **Un cambio de texto que no altera ninguna propiedad
medida no aparece**: por ejemplo, si la moda de una columna de texto
libre pasa de `"Basico"` a `"Casico"` —mismo largo, misma forma de
patrón, mismos conteos—, la comparación devuelve cero filas. En una
columna categórica sí aparece, porque su vocabulario se compara (ver más
abajo), pero sólo ahí. Por eso el remedio ante una relectura sospechosa
no es confiar en la comparación para detectarla.

El remedio es una línea, `saveRDS(perfil, archivo, version = 2)`, porque
ese formato no anota codificación nativa y por lo tanto no traduce nada.
La persistencia propia del paquete
—[`guardar_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md)
y
[`guardar_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md)—
ya lo usa. Las claves internas que agrupan hallazgos también se
normalizan por bytes; por eso dos perfiles idénticos no emiten avisos
espurios bajo C.

Que la **moda** no se compare es una **decisión**, no un pendiente: la
moda es un valor, no una propiedad medida, y cambia con cualquier
corrimiento normal de los datos, así que compararla llenaría la deriva
de filas que no son deterioro. Quien necesite detectar una relectura
corrompida tiene el remedio de una línea que está dos párrafos más
abajo, que la evita en lugar de detectarla.

El **vocabulario** de una columna categórica sí se compara: el conjunto
de sus valores, que
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
guarda en `general$vocabulario` para las columnas de texto o factor de
hasta 20 valores distintos de hasta 64 caracteres. Un faltante
disfrazado —`"NA"` escrito, un centinela— no es un valor del
vocabulario: la comparación lo informa en los faltantes y no lo cuenta
dos veces. Una entrega que cambia todos sus valores con la misma forma
—`A`/`B` por `X`/`Y`, en las mismas proporciones— no mueve la
cardinalidad, los faltantes ni los patrones, y salía sin ninguna fila.
La fila `vocabulario / modificado` nombra los valores que aparecieron y
los que desaparecieron con su proporción, y `delta` es la proporción de
filas que llevan valores que la otra entrega no tenía, la mayor de las
dos direcciones, sobre **todas** las filas de la entrega, con sus
faltantes: un valor cambiado en 1 de 1000 filas pesa 0,001 aunque las
otras 999 falten. `sospechoso` desde `umbral_cambio`, `error` desde
`umbral_error`, y `ok` por debajo, sin dejar de publicarse.

Un valor que está en una sola entrega **no cuenta para la severidad si
puede faltar de la otra por azar**: con `k` filas del valor, `n` filas
en la entrega donde aparece y `m` en la otra, si las dos salen de la
misma población las `k` caen todas del mismo lado con probabilidad
`(n / (n + m))^k`; por encima de 0,01 el valor se nombra en la evidencia
—«puede faltar del otro lado por azar»— y no gradúa la fila. Medido
sobre réplicas de una misma población —30 contra 300 filas con una
categoría del 5 %, 40 contra 40 con dos del 4 y el 6 %—, la fila pasa de
`ok` en menos del 1 % de ellas. El precio está entre entregas chicas:
una categoría nueva real del 10 % llega a `sospechoso` en menos de una
de cada diez entregas de 40 filas por lado, y en todas desde 200. Si los
dos vocabularios son el mismo salvo espacios en los bordes y mayúsculas
—lo que deja `recortar_espacios`—, la fila sale `ok` y la evidencia dice
que el vocabulario cambió sólo de forma. Por **decisión**, el
corrimiento de proporciones entre valores que siguen en las dos entregas
no gradúa la fila —sólo se informa en la evidencia, como distancia de
variación total—: la misma distribución de cuatro categorías
remuestreada en 200 filas lo mueve 0,06 de mediana, y esa alarma sería
del azar; por eso dos valores que intercambian su peso sin que aparezca
ni desaparezca ninguno no dan fila. No se compara el vocabulario de una
columna con cardinalidad alta —con el mismo criterio que su hallazgo
`alta_cardinalidad`— ni el de una cuyos valores son todos distintos, que
es un identificador y renueva sus valores en cada entrega, ni el de una
con valores de más de 64 caracteres, que es texto. **Límite declarado**:
el vocabulario se guarda hasta 20 valores distintos. Si un lado tiene
más de 20, la fila dice `no_comparable` con severidad `ok`; si los dos
tienen más de 20 no hay fila de vocabulario, así que una columna de 21
valores que reemplaza uno solo de ellos, o todos por otros de la misma
forma, puede no dar ninguna fila. Un perfil sin `general$vocabulario`
—de una versión anterior— se declara con la fila de campos ausentes, y
el de una columna protegida o sin valores publicados
(`incluir_valores = FALSE`) no se guarda y se declara en
`campos_protegidos`.

Las **marcas del plan de limpieza** —`.fila_duplicada`,
`.grupo_duplicado`, `.ausente_<columna>` y `.outlier_<columna>`— se
reconocen por lo que deja
[`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md),
no por el nombre: el atributo `marcas_limpieza` de sus datos nombra cada
marca que agregó, y
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) lo
guarda en `general$marcas_limpieza`. Una marca que aparece o desaparece
se publica con severidad `ok`, porque la tabla cambió de forma por la
limpieza que se pidió y no por la entrega, y un hallazgo sobre una marca
—los `NA` de `.grupo_duplicado` significan que la fila no está en ningún
grupo— también, con la razón en la descripción. Una columna del usuario
con uno de esos nombres es una columna del usuario: si desaparece, es
`error`, como cualquiera. El atributo sobrevive a quedarse con algunas
filas (`datos[filas, ]`, [`head()`](https://rdrr.io/r/utils/head.html),
ordenar), a [`rbind()`](https://rdrr.io/r/base/cbind.html) y a cambiar o
quitar una columna con `$<-` o `[[<-`; lo pierden la selección de
columnas con `[` (`datos[, columnas]`, `datos[columnas]`),
[`cbind()`](https://rdrr.io/r/base/cbind.html),
[`merge()`](https://rdrr.io/r/base/merge.html),
[`subset()`](https://rdrr.io/r/base/subset.html),
[`transform()`](https://rdrr.io/r/base/transform.html) y un viaje por
CSV. Sin el atributo, las marcas se comparan como columnas del usuario
—ruido, no silencio—; por eso conviene perfilar el objeto que devolvió
[`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md).

Entre las propiedades comparadas está la **unidad** en la que la columna
publica sus cifras, con la misma severidad que un cambio de tipo: dos
entregas con los mismos valores y la unidad cambiada de `m` a `km` son
un cambio real —la cifra dejó de significar lo mismo— y la comparación
lo informa en una fila. Un perfil guardado por una versión anterior del
paquete, que no registra la unidad, no hace mentir la comparación: esa
parte se declara **no comparable**, como el resto de los campos
ausentes.

El **rango** se compara por los valores de sus extremos, no por su texto
impreso: un cambio chico sale como fila `ok` y uno que alcanza
`umbral_cambio` —medido contra el ancho del rango anterior— como
`sospechoso`, sin que `options(scipen)` ni `digits` muevan el veredicto.
Cuando el perfil conserva los extremos exactos de un entero que no entra
en un doble —un `integer64` por encima de 2^53, o un entero escrito como
texto—, esos extremos se comparan con aritmética exacta. Los números se
publican con la escritura más corta que vuelve al valor, así que dos
extremos distintos nunca se imprimen iguales; la marca decimal sigue a
`OutDec`, y con coma decimal los extremos se separan con punto y coma
(`[1,5; 3]`).

Una columna **protegida** no publica su rango, y la comparación tampoco
lo compara: lo declara en el atributo `campos_protegidos`, con la misma
regla que
[`comparar_equivalencia()`](https://sebollin.github.io/lupa/reference/comparar_equivalencia.md).
Si la protección de una columna cambia entre las corridas con la misma
política —por ejemplo, porque la proporción de documentos válidos cruzó
el umbral de verificación—, el cambio se informa con una fila propia,
`aspecto = "proteccion"` y severidad `sospechoso`, en lugar de un cambio
de rango que no ocurrió en los datos. Si lo que cambió es la política
entera, ya lo dice la fila `configuracion_proteccion` y no se repite por
columna.

Además de los patrones que aparecen y desaparecen, se informa el
**cambio de proporción** de un patrón que sigue en el resumen cuando
alcanza `umbral_cambio`: `sospechoso`, o `error` desde `umbral_error`,
en las dos direcciones. Que los cambios menores no generen fila es una
**decisión**, a diferencia de los faltantes: serían una fila por patrón
en cada entrega. Si el resumen de un lado estaba **lleno** —la columna
tenía más formas que `max_patrones`—, un patrón que falta en él pudo
quedar por debajo del corte, y no se afirma que apareció o desapareció:
la fila dice `no_comparable`, con severidad `ok`, salvo que el cambio
que sí se puede garantizar —contra la menor proporción que guardaba ese
resumen— alcance el umbral; entonces dice `modificado` y `delta` es esa
cota.

La **cardinalidad** no tiene una dirección buena: que caiga la tasa de
valores distintos de un identificador es un deterioro, y que suba la de
una columna de categorías también. Por **decisión**, un cambio
significativo de cardinalidad es `sospechoso` en los dos sentidos. Los
faltantes sí la tienen: que bajen es una mejora y queda `ok`.

Las columnas que aparecen o desaparecen generan cambios estructurales de
severidad `error`, pero no impiden comparar las columnas compartidas. Un
hallazgo de una columna retirada no se presenta como resuelto.

Un hallazgo que ya no aparece se informa como `resuelto` sólo si el
diagnóstico volvió a evaluarse. Si el perfil nuevo lo declinó —y lo dice
en su `cobertura_diagnosticos`—, el cambio se informa como `no_evaluado`
con severidad `sospechoso`, porque no se sabe si el hallazgo sigue:
dejar de mirar no es lo mismo que arreglar. Una fila de esa cobertura
sin columna declina el diagnóstico en toda la tabla, también en cada
columna: un grupo de columnas iguales comparado con una entrega sin
filas sale `no_evaluado`, no `resuelto`. Lo mismo si el perfil nuevo
apagó el diagnóstico con su argumento o retiró la declaración que lo
habilita, cosa que se lee en `meta`:
`casi_duplicados_vocabulario = FALSE`, `ausencia_estructural = FALSE`,
`variantes_equifrecuentes_vocabulario = FALSE`,
`analizar_dependencias = FALSE`, una `clave` retirada o declarada sobre
otras columnas, una columna retirada de `columnas_sin_ceros`,
`columnas_no_negativas`, `columnas_personales` o `aplicabilidad`, y
`duplicados_aproximados` apagado o corrido sobre otras columnas —lo que
corrió queda en `perfil$duplicados_aproximados`; si corrió sobre las
mismas columnas con otros parámetros, la fila
`configuracion_duplicados_aproximados` lo declara—; la descripción
nombra el argumento. Al revés, un hallazgo que aparece porque el perfil
nuevo encendió o declaró lo que lo habilita sigue saliendo `aparecido`,
y su descripción dice que el anterior no lo evaluaba. Un hallazgo de
varias columnas —un grupo de columnas duplicadas, una relación entre
columnas— se compara por el contenido de sus columnas, no por sus
posiciones en la tabla: reordenar columnas no hace aparecer ni
desaparecer un grupo. Sólo no se presenta como resuelto si alguna de sus
columnas desapareció. Un grupo de columnas duplicadas que se separa sólo
porque una de sus columnas cambió de tipo declarado —un entero que llega
como doble— se publica `no_evaluado`, con severidad `sospechoso`: el
diagnóstico sólo iguala columnas del mismo tipo y no volvió a
compararlas, salvo que el perfil nuevo ya muestre valores distintos
(otros faltantes, otra cantidad de distintos, otro mínimo o máximo); y
uno que aparece sólo por un cambio de tipo sale `aparecido` con esa
causa en la descripción. Si el tope de un diagnóstico deja un hallazgo
fuera del alcance nuevo, se publica `no_evaluado` con el motivo de
cobertura. La proximidad de vocabulario también puede dejar pares con
frecuencias parejas bajo su piso de asimetría sin emitir un hallazgo;
esa cobertura parcial se conserva como `no_evaluado` al comparar, aunque
el diagnóstico haya recorrido todos sus valores. Cada argumento de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
tiene su lugar en la comparación: habilita un diagnóstico, es una vara
—su cambio se declara en la fila `configuracion_umbrales`, que incluye
los umbrales de las relaciones de orden y aritméticas entre columnas y
de las dependencias funcionales, los del vocabulario y
`datos_personales_permitidos`, `validadores_personales`,
`umbral_documento_verificado` y `muestra_validadores`—, tiene su propia
fila de configuración, acota el trabajo y lo declara en
`cobertura_diagnosticos`, o no cambia los hallazgos. Una vara que uno de
los dos perfiles no registra —un perfil de una versión anterior— no se
compara. La comparación no sabe de versiones: un perfil guardado por una
versión que todavía no emitía un tipo de hallazgo no lo trae, y
comparado con uno nuevo de los mismos datos ese hallazgo sale
`aparecido`. Pasa con `relacion_funcional_columnas` frente a un perfil
anterior a ese hallazgo: una fila `aparecido` por cada dependencia casi
exacta, en cada sentido.

Los hallazgos que cuentan filas de la tabla y no pertenecen a una
columna concreta tampoco se declaran resueltos si aparecieron o
desaparecieron columnas entre las corridas: cambio el universo que
define esas filas. En ese caso se publica `no_comparable`, con severidad
`error`, y se nombran las columnas cambiadas. Con las mismas columnas,
el veredicto `resuelto` se conserva.

## Perfiles de una base de datos

Un objeto de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
trae dos bloques con universos distintos: el resumen SQL
(`resumen_tabla`), que calcula el motor, y el perfil de muestra
(`perfil_muestra`), que calcula R sobre las filas leídas. Con
`muestra = Inf` —el valor por omisión— la muestra cubre la tabla y la
comparación es la misma que entre sus dos perfiles de muestra.

Si la muestra de algún lado no cubre la tabla, cada aspecto se toma del
universo más amplio que lo mide. El rango de las columnas numéricas y la
cardinalidad salen del resumen SQL cuando la cifra tiene estado
`calculado` sobre la tabla completa en los dos lados; del lado de un
perfil en memoria, de ese perfil. Los faltantes conservan su definición
—los `NULL` y los disfrazados—: los primeros los cuenta el motor sobre
la tabla y los segundos se estiman sobre la muestra, porque el resumen
SQL no reconoce centinelas ni cadenas de ausencia; por eso **un faltante
disfrazado que entra en filas que la muestra no lee no aparece**, y la
fila `muestra` lo dice. Las dos partes se suman sólo si miden sobre el
mismo denominador: con una declaración de aplicabilidad o de columnas
opcionales —que el resumen SQL no aplica— queda la estimación de la
muestra, rotulada `muestra`. Los patrones, los hallazgos y el tipo
inferido sólo los mide la muestra, y no se comparan como cambios de la
tabla. Los extremos de una fecha —también la que se guarda como texto—
se compararían si el resumen SQL los calculara en los dos lados; no los
calcula, y la fila `rango / no_comparable` lo dice con el motivo
`sin_cifra_sql:el_resumen_sql_no_calcula_minimo_fecha_ni_maximo_fecha`.
El vocabulario tampoco: el resumen SQL no guarda valores, y sus columnas
se declaran en una fila `vocabulario / no_comparable` con el motivo
`sin_cifra_sql:el_resumen_sql_no_guarda_valores`.

Una cifra SQL que en algún lado no se calculó sobre la tabla completa
—`no_disponible`, `no_solicitado`, `omitido_por_costo`, `estimado_motor`
(también contra otra estimación del motor), las de
`universo = "muestra_motor"`— no se compara: una fila `no_comparable`
por aspecto, con `columna` en `NA`, nombra en `evidencia` cada columna
con su motivo, `estado_sql:<anterior>_vs_<actual>` o
`universo_distinto:<anterior>_vs_<actual>`, con lo que el motor dijo de
la cifra y, si un lado no la pidió, la métrica de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
que falta (por ejemplo `basicos` para el rango). La que la protección
omitió se declara en `campos_protegidos`.

El rango y la cardinalidad del resumen SQL se comparan sólo entre
columnas de la misma familia de tipo —número, texto, fecha, lógico—: un
booleano contra un entero `0/1`, o un entero que pasa a texto con las
mismas cifras, no tienen un rango que comparar, y el motivo es
`tipo_distinto:<anterior>_vs_<actual>`; la fila de `tipo_declarado` ya
dice el cambio.

La cardinalidad de una columna de texto sólo se compara entre dos
objetos del mismo controlador cuya columna tiene el mismo cotejamiento:
el motor cuenta valores distintos con el cotejamiento de la columna —que
puede ignorar mayúsculas o acentos— y R por bytes, y los mismos datos
con `COLLATE NOCASE` y con `COLLATE BINARY` darían una deriva falsa.
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
lee el cotejamiento de cada columna en SQLite y en DuckDB
(`meta$cotejamientos` del resumen); en otro motor, o contra un perfil en
memoria, la cardinalidad de texto se declara `no_comparable` con el
motivo `cotejamiento_no_verificado:...`, y con cotejamientos distintos,
`cotejamiento_distinto:<anterior>_vs_<actual>`.

Para comparar una entrega en la base con otra en un archivo, conviene
leer el archivo con los tipos de la base: un número entero en un CSV se
relee como entero, y el cambio de tipo se publica aunque los datos sean
los mismos.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md),
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md),
[`comparar_equivalencia()`](https://sebollin.github.io/lupa/reference/comparar_equivalencia.md)

## Examples

``` r
anterior <- perfilar(data.frame(codigo = c("AA1", "AA2")),
                     fecha = as.POSIXct("2026-01-01", tz = "UTC"))
actual <- perfilar(data.frame(codigo = c("AA1", "B-2"), nueva = 1:2),
                   fecha = as.POSIXct("2026-02-01", tz = "UTC"))
comparar_perfiles(anterior, actual)
#>   columna aspecto     cambio severidad valor_anterior valor_actual delta
#> 1   nueva columna  aparecida     error           <NA>        nueva    NA
#> 2  codigo  patron  aparecido     error           <NA>          A-9   0.5
#> 3  codigo  patron modificado     error            A+9          A+9  -0.5
#>   cambio_relativo significativo fecha_anterior fecha_actual
#> 1              NA          TRUE     2026-01-01   2026-02-01
#> 2              NA          TRUE     2026-01-01   2026-02-01
#> 3              NA          TRUE     2026-01-01   2026-02-01
#>                                                        descripcion
#> 1       Apareció una columna que no estaba en la entrega anterior.
#> 2                             Apareció un patrón de formato nuevo.
#> 3 Cambió la proporción de un patrón de formato que sigue presente.
#>                                        evidencia universo_anterior
#> 1 No existe una base anterior para sus métricas.    tabla_completa
#> 2                      Proporción actual: 0.5000    tabla_completa
#> 3                  Proporción: 1.0000 -> 0.5000.    tabla_completa
#>   universo_actual
#> 1  tabla_completa
#> 2  tabla_completa
#> 3  tabla_completa
```
