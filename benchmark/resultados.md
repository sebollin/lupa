# Resultado publicado

> **Cuando es cada cosa de esta pagina, y por que importa.** No hay una sola
> fecha: la reproduccion de Raha es del **2026-08-18**; la tabla de PED y RIOLU,
> del **2026-08-22**; el desglose de PED/Hospital, del **2026-08-24**. Cada
> seccion que se rehizo lo declara arriba. La cabecera decia «2026-08-18» a
> secas y fechaba mal todo el documento.
>
> **Y hay algo mas serio que la fecha.** Desde el 2026-08-24, **19 commits
> tocaron los archivos de `R/` cuyos diagnosticos mide esta pagina**
> -`hallazgos.R`, `duplicados-aproximados.R`, `patrones.R`, `columnas.R`-. Las
> cifras de precision y cobertura de PED y RIOLU describen el paquete de esa
> fecha, no el de hoy. Esta pagina ya escribio la regla que ahora le toca:
> «un desglose que no se rehace despues de un cambio asi describe un paquete que
> ya no existe». **Rehacerlas esta pendiente**; se rehacen con
> `benchmark/medir_bancos.R` y `benchmark/_ped_desglose.R`, que necesitan las
> copias locales (`PED_DATA_DIR`, `RIOLU_DATA_DIR`) o red.
>
> Lo que **si** esta medido sobre el codigo de hoy es la cobertura de columnas
> de Raha -26 de 26-, que rehace `revalidar.sh` en cada corrida.

- Version declarada de `lupa`: `0.1.0`.
- Las huellas de archivos son Adler-32 calculado en R base. No son
  criptograficas y solo identifican cambios de los bytes obtenidos.
- `benchmark/` esta fuera del tarball; los datos externos tampoco se
  redistribuyen.

## Que cuenta cada numero

En los pares dirty/clean, `celdas diferentes` cuenta posiciones alineadas cuyo
texto no coincide; `filas afectadas` cuenta filas con al menos una posicion
diferente; `columnas afectadas` cuenta columnas con al menos una posicion
diferente. La tasa usa todas las celdas de la tabla como denominador.

En la medicion de `lupa`, `columnas con hallazgo` cuenta columnas que tienen al
menos un hallazgo de severidad distinta de `ok`. Por tanto, `26 de 26` es
**cobertura de columnas afectadas**, no recall diagnostico ni deteccion de cada
celda.

Cuando un banco permite trazar filas, `celdas con hallazgo trazable` cuenta la
union de posiciones que `lupa` adjunta a esos hallazgos. La interseccion con la
verdad se llama `aciertos trazables`; sus dos cocientes son precision de celdas
trazables y cobertura de celdas de verdad. No se publican como recall porque la
trazabilidad de un hallazgo por columna no es una etiqueta causal de una celda.

## Raha: dirty contra clean

La corrida de `verdad_raha.R` sobre la copia local disponible reprodujo los
conteos publicados por Raha. Se leyeron los seis CSV como texto, conservando
las cadenas vacias.

| Dataset | Dimension | Celdas diferentes | Filas afectadas | Tasa sobre celdas | Columnas afectadas | Celdas vacias en dirty con valor en clean | Duplicados exactos en dirty |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| hospital | 1000 x 20 | 509 | 407 | 2,545 % | 17 | 0 | 0 |
| flights | 2376 x 7 | 4920 | 1904 | 29,582 % | 4 | 2312 | 0 |
| beers | 2410 x 11 | 4362 | 2410 | 16,454 % | 5 | 127 | 0 |

Desglose de celdas diferentes por columna:

| Dataset | Columna | Celdas diferentes |
| --- | --- | ---: |
| hospital | provider_number | 28 |
| hospital | name | 24 |
| hospital | address_1 | 31 |
| hospital | city | 33 |
| hospital | state | 26 |
| hospital | zip | 30 |
| hospital | county | 39 |
| hospital | phone | 34 |
| hospital | type | 32 |
| hospital | owner | 27 |
| hospital | emergency_service | 27 |
| hospital | condition | 32 |
| hospital | measure_code | 29 |
| hospital | measure_name | 36 |
| hospital | score | 23 |
| hospital | sample | 31 |
| hospital | state_average | 27 |
| flights | sched_dep_time | 911 |
| flights | act_dep_time | 1558 |
| flights | sched_arr_time | 1100 |
| flights | act_arr_time | 1351 |
| beers | ounces | 2410 |
| beers | abv | 693 |
| beers | ibu | 1005 |
| beers | city | 127 |
| beers | state | 127 |

La suma por columna es el conteo de celdas diferentes del dataset, no el
numero de hallazgos de `lupa`.

## Cobertura de columnas de `lupa` sobre Raha

Esta tabla conserva la medicion publicada con una instalacion de `lupa` que
paso la sonda de capacidad de `medir_lupa.R`. La referencia sigue siendo el
conjunto de columnas con al menos una diferencia dirty/clean.

| Dataset | Columnas afectadas | Columnas con hallazgo | Cobertura de columnas afectadas |
| --- | ---: | ---: | ---: |
| hospital | 17 | 17 | 17/17 |
| flights | 4 | 4 | 4/4 |
| beers | 5 | 5 | 5/5 |
| **Total** | **26** | **26** | **26/26** |

Se señalaron además 8 columnas que no tienen diferencias dirty/clean. La
comparacion dirty/clean no las convierte en falsos positivos: fueron revisadas
como observaciones verdaderas sobre constantes, columnas duplicadas,
mayusculas inconsistentes, cadenas vacias y cardinalidad de texto. Esas ocho
observaciones quedan fuera de la cobertura de columnas afectadas.

## Donde coinciden las unidades y donde no

Los conteos por celda de Raha y los conteos internos de un hallazgo de `lupa` no
son la misma unidad. En la corrida documentada, la coincidencia exacta se dio
solo en `beers/ibu`: 1005 celdas de `N/A` en dirty frente a ausencia real en
clean, que `lupa` conto como faltantes disfrazados.

No coinciden en `beers/ounces`: las 2410 celdas cambian de representaciones como
`12.0 oz.` a `12`; `lupa` las informa como unidad y numero escrito como texto,
no como faltantes. Tampoco se puede leer el conteo de un hallazgo sobre
`flights/act_dep_time` o `flights/sched_dep_time` como las 1558 o 911 celdas de
Raha: el hallazgo caracteriza una propiedad de la columna y puede cubrir solo
una parte de esas posiciones. En `beers/state`, la diferencia dirty/clean es
127 celdas porque el estado fue absorbido por `city`; `lupa` informa la
estructura observada con su propio conteo de filas, no una correspondencia
uno-a-uno con esas 127 posiciones.

## PED y RIOLU: lo medido

Medicion del **2026-08-22** sobre una copia local descargada para medir y **no
redistribuida**. Se repite sin red con `PED_DATA_DIR` y `RIOLU_DATA_DIR`.

> **Esta tabla se rehizo el 2026-08-22 y cuatro de sus siete filas cambiaron.**
> La medicion anterior, del 2026-08-18, quedo vieja al corregirse el recorte del
> vocabulario: se elegian las formas a comparar por orden de llegada y ahora se
> ordenan antes de recortar. Eso cambia que se compara, y por lo tanto que se
> encuentra. Los numeros viejos no eran falsos cuando se publicaron; dejaron de
> valer y **nadie se entero hasta que una auditoria los volvio a correr**, que es
> justamente lo que esta pagina existe para evitar.
>
> | conjunto | antes (prec / cob) | ahora |
> | --- | ---: | ---: |
> | PED / Hospital | 0,043 / 0,338 | 0,035 / **0,821** |
> | PED / Flight | 0,819 / 0,171 | 0,658 / 0,238 |
> | RIOLU / flights | 0,369 / 0,027 | 0,501 / 0,054 |
> | RIOLU / movies | 0,334 / 0,445 | 0,338 / 0,494 |
> | los tres `hosp_*` | sin cambio | sin cambio |
>
> **La cobertura sube en las cuatro que cambiaron; la precision baja en dos**, y
> en PED/Flight baja fuerte. El paquete encuentra bastante mas, y una porcion
> mayor de lo que encuentra no coincide con la verdad declarada. Se publica asi
> y no como una mejora limpia.
>
> Con un matiz que vale para PED y que esta escrito mas arriba para Raha: la
> verdad de PED es la diferencia `dirty`/`clean`, que **solo etiqueta celdas
> cambiadas**. Una parte de esos "falsos positivos" pueden ser problemas reales
> que la verdad no etiqueta. No esta comprobado que lo sean, asi que se cuentan
> como fallos.

La unidad de `PED` es la posicion `(fila, columna)` listada en `difference.csv`
(`Index`, `Attribute`). La unidad de `RIOLU` es la posicion cuya etiqueta en
`gt_*.csv` es exactamente `1`; `0` y `-1` no cuentan. En ambos casos `dirty.csv` y
`clean.csv` solo comprueban que la copia este alineada: no reemplazan la verdad
publicada.

| Banco / conjunto | Filas x col | % celdas corruptas | Celdas de verdad | Trazables | Aciertos | Precision | Cobertura | Techo | Cobertura / techo |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| PED / Hospital | 1000 x 19 | 2,7 % | 509 | 11795 | 418 | 0,035 | 0,821 | 0,762 | — |
| RIOLU / movies | 7390 x 5 | 4,5 % | 1321 | 1930 | 652 | 0,338 | 0,494 | 1,000 | 49 % |
| PED / Flight | 2376 x 6 | 18,3 % | 2608 | 946 | 622 | 0,524 -> **0,658** | 0,281 -> 0,238 | 0,423 | — |
| RIOLU / hosp_10k | 10000 x 7 | 24,3 % | 7289 | 1550 | 1406 | 0,907 | 0,193 | 0,608 | 32 % |
| RIOLU / hosp_1k | 999 x 7 | 25,2 % | 755 | 156 | 144 | 0,923 | 0,191 | 0,656 | 29 % |
| RIOLU / flights | 74066 x 6 | 52,5 % | 233173 | 25088 | 12558 | 0,501 | 0,054 | 1,000 | 5 % |
| RIOLU / hosp_100k | 100000 x 7 | 53,4 % | 160186 | 14700 | 13422 | 0,913 | 0,084 | 0,311 | 27 % |

### Los pares que la columna no puede distinguir

Mirando **que** reportaba el detector de vocabulario en `PED/Flight` aparecieron
dos familias mezcladas:

```
[1:48 p.m. (27)  / 1:48 p.m.            Delayed (1)]   <- el estado del vuelo pegado
[11:08 p.m. (20) / 11:08 p.m.            On Time (1)]     dentro de una columna de hora
[12:00 a.m. (5)  / 12:00 p.m. (42)]                    <- doce horas de diferencia
[7:10 a.m. (100) / 7:10 p.m. (18)]
```

Las dos primeras son hallazgos reales que la verdad de PED **no etiqueta**,
porque no son erratas inyectadas: estaban en los datos. Las dos ultimas son
falsos positivos: `12:00 a.m.` y `12:00 p.m.` son **dos valores legitimos
distintos**, y no hay forma de saber mirando la columna cual fue tipeado mal.

Marcarlos a todos no es detectar: es sospechar en bloque de todos los valores de
una forma y acertar los inyectados por casualidad. La precision de ese
diagnostico sobre `Flight` era **0,259**, o sea tres de cada cuatro marcados eran
valores legitimos.

Ahora el detector descarta un par cuando **todos los tokens que lo distinguen
aparecen en buena parte de la columna**: son marca de formato y no una errata.
`a.m.` y `p.m.` estan en casi todos los valores; `Delayed` esta en uno. El
descarte se declara en `n_pares_descartados_formato`.

**El costo, medido y a la vista:** sobre `Flight` la precision sube de 0,524 a
0,658 y la cobertura baja de 0,281 a 0,238. Se pierden 111 aciertos, porque PED
inyecto erratas que son exactamente un cambio de meridiano. **Esas caen debajo
del techo estructural**: un `p.m.` mal tipeado es indistinguible de uno correcto
sin una referencia externa, igual que el codigo postal de cinco digitos. El
lugar correcto para atraparlas no es la proximidad de cadenas sino una regla
entre columnas -que la llegada no sea anterior a la salida-, que es otro
diagnostico.

Sobre `Hospital` el descarte **no cambia nada**: lo que distingue dos nombres de
hospital es contenido, no una marca de formato. La regla actua solo donde la
marca existe.

**El techo no acota a PED, y nunca lo acoto.** El techo es la fraccion de celdas
de verdad cuyo valor corrupto **cambia el patron** de la columna: es una cota para
un metodo basado en forma. Los conjuntos de `RIOLU` se miden restringiendo `lupa`
a `patron_raro`, asi que ahi el techo es la cota correcta. Los de `PED` se miden
con **todos** los diagnosticos —`constante`, `numero_como_texto`,
`unidades_mixtas` y los demas encuentran cosas que no son anomalias de forma—,
asi que el techo nunca fue su cota. Era un error de categoria que la medicion
vieja no dejaba ver, y la nueva si: en PED/Hospital la cobertura (0,821)
**supera el techo** (0,762). Por eso la razon `Cobertura / techo` no se informa
para las dos filas de PED.

Ademas, **ningun script del repositorio calcula el techo**: `medir_bancos.R` no
emite esa columna. Los valores que quedan son los de la medicion del 2026-08-18,
heredados. Hasta que exista el codigo que los reproduzca, la columna `Techo` es
un numero viejo y no una medicion vigente, y esta pagina no puede pedirle al
lector que confie en el.

### El techo estructural, y por que la cobertura no se lee sin el

**`Techo`** es la fraccion de las celdas de verdad cuyo valor corrupto **cambia el
patron** de la columna. El resto conserva la forma dominante y es invisible para
cualquier metodo basado en forma, `lupa` incluido.

En la columna `zip` de RIOLU el techo es **cero**: los 46 valores corruptos siguen
teniendo cinco digitos. Un codigo postal equivocado es indistinguible de uno
correcto sin un padron externo de codigos validos. Lo mismo con `LA` en lugar de
`AL`, o con un telefono al que se le cambio un digito sin cambiar el largo.

Publicar la cobertura sin el techo hace parecer un fracaso lo que es un limite del
metodo. `0,191` contra un techo de `0,656` significa **capturar el 29 % de lo
capturable**, no fallar el 81 %.

### Las dos precisiones no son comparables entre si

Hay una asimetria deliberada en el alcance que **hay que leer antes que los
numeros**: los conjuntos de `RIOLU` se miden restringiendo `lupa` a `patron_raro`,
porque la verdad de ese banco son anomalias de patron; los de `PED` se miden con
**todos** los diagnosticos, porque su verdad son las celdas que difieren entre la
copia sucia y la limpia.

Poner `0,923` al lado de `0,043` sin decir esto invita a una conclusion falsa.

### Por que la precision de PED/Hospital es baja, y por que no es un error

> **Rehecho el 2026-08-24** con `benchmark/_ped_desglose.R`, que lo reproduce
> desde el repositorio. El desglose anterior era del 2026-08-18 y sumaba 4.035
> celdas contra las 11.795 de hoy.

Las celdas trazables de esa tabla se reparten asi:

| diagnostico | celdas trazables |
| --- | ---: |
| `casi_duplicados_vocabulario` | 8056 |
| `constante` | 2000 |
| `numero_como_texto` | 1761 |
| `unidades_mixtas` | 1759 |
| `patron_raro` | 216 |
| `outliers` | 99 |
| `codificacion_rota` | 1 |

**Una celda trazable es un par (fila, columna), y el total es su union, no la
suma de la tabla.** La suma da 13.892 porque dos diagnosticos pueden senalar la
misma celda; la union da 11.795, que es la cifra de la fila. Contar filas en vez
de celdas daria 1.000 —todas—, que no dice nada.

**Cuatro diagnosticos aportan el 97 % y los cuatro dicen algo verdadero sobre la
tabla, ajeno a lo que el banco etiqueta.** Dos columnas efectivamente tienen un
unico valor; los numeros efectivamente estan guardados como texto; las unidades
efectivamente estan mezcladas; y el vocabulario efectivamente tiene formas casi
iguales. Nada de eso es una errata inyectada, que es lo que `difference.csv`
marca.

**El mas grande cambio de dueno al rehacer el desglose**: en la medicion vieja
encabezaba `constante` con 2.000 celdas, y hoy lo hace
`casi_duplicados_vocabulario` con 8.056 —dos tercios del total—. No es que el
paquete empeorara: es que el detector de vocabulario paso a trazar sus filas, y
lo que antes no se contaba ahora se cuenta. Un desglose que no se rehace despues
de un cambio asi describe un paquete que ya no existe.

Es un desajuste de **categoria**, no de acierto. La medida lo confirma: restringido
a `patron_raro`, el mismo conjunto pasa de **0,043 a 0,711** de precision.

### La ventana de operacion de `patron_raro`, medida

El diagnostico de patrones exige **dos condiciones a la vez**, y entre ellas queda
una franja estrecha que conviene tener presente antes de leer cualquier cifra de
cobertura:

- un patron dominante que ocupe al menos `umbral_patron_dominante` (**0,5** por
  omision) de la columna;
- y una forma corrupta que ocupe menos de `umbral_patron_raro` (**0,05**).

**Fuera de esa franja el diagnostico no aplica.** Y eso explica el orden de la
tabla mejor que la densidad de corrupcion. Proporcion del patron dominante, medida
columna por columna:

| conjunto | columna | dominante | ¿opera? |
| --- | --- | ---: | --- |
| RIOLU / flights | `ScheduledDeparture time` | 0,324 | **no** |
| RIOLU / flights | `ActualDeparture time` | 0,157 | **no** |
| RIOLU / flights | `ScheduledArrival time` | 0,319 | **no** |
| RIOLU / flights | `ActualArrival time` | 0,163 | **no** |
| RIOLU / flights | `DepartureGate` | 0,601 | si |
| RIOLU / flights | `ArrivalGate` | 0,599 | si |
| RIOLU / hosp_1k | `state`, `zip`, `phone` | 0,81 a 1,00 | si |
| RIOLU / movies | las cuatro etiquetadas | 0,85 a 0,97 | si |

**Cuatro de las seis columnas de `flights` no tienen patron dominante.** Son horas
y puertas de embarque, cuya forma varia de por si. Por eso su cobertura es 0,027 y
no por lo que se afirmaba antes en este documento: la explicacion anterior decia
que con mas de la mitad de las celdas corruptas la forma dominante pasa a ser la
corrupta y el supuesto se invierte. **Esa inversion existe pero exige alrededor del
97 % de corrupcion**, no del 50 %; a 52,5 % el diagnostico simplemente no habla.

### Tres limites medidos, que valen como condiciones de uso

**Uno. En columnas de forma naturalmente variable, el diagnostico no aplica.**
Misma corrupcion, mismas densidades, cambiando solo la base:

| base | dominante | 2 % | 10 % | 20 % |
| --- | ---: | ---: | ---: | ---: |
| codigos tipo `ABC0001` | 1,000 | 1,000 | 0 | 0 |
| direcciones reales | 0,468 | **0** | **0** | **0** |

Nombres, direcciones y texto libre quedan fuera. Codigos, matriculas, telefonos e
identificadores son su terreno.

**Dos. Cuando la corrupcion se concentra en una forma, la magnitud informada
subestima.** Corrupcion total del 8 % repartida en cinco formas:

| peso de la forma mayoritaria | corruptas reales | `n_afectados` |
| ---: | ---: | ---: |
| 20 % | 160 | 160 |
| 50 % | 160 | 160 |
| 80 % | 160 | **32** |
| 95 % | 160 | **8** |

La forma mayoritaria supera ella misma el umbral de rareza y queda excluida por no
ser rara. El conteo no miente sobre lo que midio, pero la desproporcion importa, y
en datos reales la corrupcion suele concentrarse asi.

**Tres. Con la columna casi toda corrupta, el diagnostico se invierte.** A partir
de alrededor del 97 % de una sola forma corrupta, esa forma pasa a ser el dominante
y el hallazgo enumera **las filas limpias** como raras. Medido: a 97 %, sesenta
filas nombradas, todas correctas, cero corruptas. No es reparable ajustando
umbrales: con esa proporcion nada en la columna dice cual era la forma correcta.

### Las perillas se midieron y no se movieron

Se barrieron `expandir` y `umbral_patron_raro` en los siete conjuntos. Sobre RIOLU,
`expandir = TRUE` con `umbral = 0,10` mejora **las dos** metricas a la vez —en
`hosp_1k`, de `0,923 / 0,191` a `0,955 / 0,334`— y no agrega un solo falso positivo
sobre la bateria de 31 tablas limpias.

**Sobre PED hace lo contrario**: cambia cobertura por precision. En `Flight` va de
`0,819 / 0,171` a `0,945 / 0,166`.

Por eso **los valores por omision no se cambiaron**. Calibrarlos contra el banco
que se reporta es sobreajuste, y la evidencia disponible apunta en dos direcciones
segun el banco.

### Completitud: contraste contra un generador independiente

El paquete `messy` genera desorden con un catalogo escrito por terceros, para otro
fin, de modo que no puede estar ajustado a lo que `lupa` detecta. Se aplico cada
una de sus transformaciones por separado sobre una tabla limpia y se comprobo si
`lupa` produce el diagnostico especifico correspondiente:

| transformacion | diagnostico esperado | resultado |
| --- | --- | --- |
| `add_special_chars` | `patron_raro` | detectado |
| `add_whitespace` | `espacios_sobrantes` | detectado |
| `change_case` | `mayusculas_inconsistentes` | detectado |
| `duplicate_rows` | `filas_duplicadas` | detectado |
| `make_missing` | `faltantes` | detectado |
| `messy_colnames` | `nombres_columnas_problematicos` | detectado |
| `messy_date_formats` | `formatos_fecha_mixtos` | detectado |
| `split_dates` | `fecha_partida_columnas` | detectado |

**Ocho de ocho.** No se publican cifras de precision contra `messy`: es sintetico y
su modelo de corrupcion es propio, asi que serviria para medir contra un blanco que
podriamos moldear. Se usa solo como control de completitud.

### No medidos de PED, con su razon

| Fuente | Razon |
| --- | --- |
| PED / MIMIC, PED / Plane | `difference.csv` no coincide con las celdas que difieren entre `dirty.csv` y `clean.csv` |
| PED / Soccer | los archivos no estan publicados en la ruta declarada |

Ninguna fila se publica sin razon declarada. `TableEG` y `AddressTable` tienen su
propia seccion mas abajo.

## TableEG y AddressTable: intento y motivo de no medicion

En [TableEG](https://github.com/viviancircle/TableEG) se reviso el README y la
[carpeta de Google Drive](https://drive.google.com/drive/folders/10LdB9LGgymbI6W8D2936uRF6eFRL24xy?usp=sharing).
La fuente publica una carpeta, no una URL directa de CSV/ZIP reutilizable por
R base. El script intenta obtener la pagina HTML de esa carpeta; no la trata
como un archivo de datos ni descarga a ciegas todo su contenido. Requiere
`TABLEEG_DATA_DIR` o un ZIP directo indicado por `TABLEEG_ARCHIVE_URL`. No se
declara licencia de datos y no se redistribuye.

Para [AddressTable](https://zenodo.org/records/20841898) se intentan la API de
metadata de Zenodo y la pagina del registro, pero no se baja el archivo
completo: la publicacion contiene archivos de varios GB. Sin una muestra local
o dos URLs directas autorizadas (`ADDRESSTABLE_FILE_URL` y
`ADDRESSTABLE_CLEAN_URL`) no hay un alcance medible. La licencia especifica de
los datos debe tomarse del campo Rights del registro; no se redistribuye.

Por tanto, esta actualizacion no agrega numeros de TableEG ni AddressTable.
La unica medicion externa numerica que permanece publicada en esta corrida es
la reproduccion anterior de Raha, cuyo alcance esta declarado arriba.

## Version de los archivos de Raha

| Dataset | Archivo | Bytes | Adler-32 |
| --- | --- | ---: | --- |
| hospital | dirty.csv | 303306 | `f970da05` |
| hospital | clean.csv | 303324 | `74435416` |
| flights | dirty.csv | 154776 | `d3a57255` |
| flights | clean.csv | 173159 | `ffe72679` |
| beers | dirty.csv | 255295 | `b2bb7910` |
| beers | clean.csv | 233019 | `71980a46` |

La huella no es criptografica: permite detectar que cambiaron los bytes
obtenidos sin prometer una funcion criptografica portable en R base.
