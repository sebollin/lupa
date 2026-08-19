# Resultado publicado

- Fecha de esta actualizacion: 2026-08-18.
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

Medicion del 2026-08-18 sobre una copia local descargada para medir y **no
redistribuida**. Se repite sin red con `PED_DATA_DIR` y `RIOLU_DATA_DIR`.

La unidad de `PED` es la posicion `(fila, columna)` listada en `difference.csv`
(`Index`, `Attribute`). La unidad de `RIOLU` es la posicion cuya etiqueta en
`gt_*.csv` es exactamente `1`; `0` y `-1` no cuentan. En ambos casos `dirty.csv` y
`clean.csv` solo comprueban que la copia este alineada: no reemplazan la verdad
publicada.

| Banco / conjunto | Filas x col | % celdas corruptas | Celdas de verdad | Trazables | Aciertos | Precision | Cobertura | Techo | Cobertura / techo |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| PED / Hospital | 1000 x 19 | 2,7 % | 509 | 4035 | 172 | 0,043 | 0,338 | 0,762 | 44 % |
| RIOLU / movies | 7390 x 5 | 4,5 % | 1321 | 1763 | 588 | 0,334 | 0,445 | 1,000 | 45 % |
| PED / Flight | 2376 x 6 | 18,3 % | 2608 | 546 | 447 | 0,819 | 0,171 | 0,423 | 40 % |
| RIOLU / hosp_10k | 10000 x 7 | 24,3 % | 7289 | 1550 | 1406 | 0,907 | 0,193 | 0,608 | 32 % |
| RIOLU / hosp_1k | 999 x 7 | 25,2 % | 755 | 156 | 144 | 0,923 | 0,191 | 0,656 | 29 % |
| RIOLU / flights | 74066 x 6 | 52,5 % | 233173 | 16889 | 6240 | 0,369 | 0,027 | 1,000 | **3 %** |
| RIOLU / hosp_100k | 100000 x 7 | 53,4 % | 160186 | 14700 | 13422 | 0,913 | 0,084 | 0,311 | 27 % |

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

Las celdas trazables de esa tabla se reparten asi:

| diagnostico | celdas trazables |
| --- | ---: |
| `constante` | 2000 |
| `numero_como_texto` | 1761 |
| `unidades_mixtas` | 1759 |
| `patron_raro` | 197 |
| `outliers` | 99 |
| `codificacion_rota` | 1 |

**El 95 % viene de tres diagnosticos que dicen algo verdadero sobre la tabla y
ajeno a lo que el banco etiqueta.** Dos columnas efectivamente tienen un unico
valor; los numeros efectivamente estan guardados como texto; las unidades
efectivamente estan mezcladas. Nada de eso es una errata inyectada, que es lo que
`difference.csv` marca.

Es un desajuste de **categoria**, no de acierto. La medida lo confirma: restringido
a `patron_raro`, el mismo conjunto pasa de **0,043 a 0,711** de precision.

### El limite duro: cuando la corrupcion es mayoria

`RIOLU / flights` es el unico conjunto donde `lupa` anda claramente mal: precision
`0,369` y cobertura `0,027` **contra un techo de 1,000**, o sea que las anomalias si
cambian el patron y aun asi no las ve.

La causa esta en la tercera columna de la tabla: **el 52,5 % de sus celdas estan
corruptas**. El analisis de patrones supone que **la forma dominante es la
correcta**. Con mas de la mitad de las celdas mal, la forma dominante *es* la
corrupta, y el supuesto se invierte.

Es una condicion de uso, no una excusa: sobre una tabla con la mitad de las celdas
corruptas, este metodo no aplica.

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
