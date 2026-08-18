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

## Bancos adicionales: estado de disponibilidad

No se inventa un numero cuando no hubo una copia alineada. Las siguientes
salidas son estados de esta corrida; los scripts quedan preparados para
repetirlos sin red mediante las variables indicadas.

| Banco | Estado | Fuente y condiciones de descarga | Motivo de no publicar una medicion |
| --- | --- | --- | --- |
| PED | No medido | [Repositorio PED](https://github.com/twinklelittlestars/PED), archivos `dirty.csv`, `clean.csv` y `difference.csv`; `PED_DATA_DIR` para copia local; `PED_DATASETS` controla la seleccion. No se encontro licencia de datos declarada; no se redistribuye. | La descarga de raw GitHub no resolvio el host en esta corrida. El script deja el error de cada archivo. |
| TableEG | No medido | [Repositorio TableEG](https://github.com/viviancircle/TableEG) y su [carpeta de datos](https://drive.google.com/drive/folders/10LdB9LGgymbI6W8D2936uRF6eFRL24xy?usp=sharing); se requiere `TABLEEG_DATA_DIR` o `TABLEEG_ARCHIVE_URL`. No se encontro licencia de datos declarada; no se redistribuye. | La fuente ofrece una carpeta, no una URL directa de archivo que el script pueda bajar con R base; no se trato una pagina HTML como si fuera un ZIP. |
| RIOLU | No medido | [Replicacion RIOLU](https://github.com/mooselab/Discover-Data-Quality-With-RIOLU), datos `dirty_*`, `clean_*`, `gt_*`; `RIOLU_DATA_DIR` para copia local. El codigo declara MIT, pero no una licencia separada para los datos; no se redistribuyen. | Las tres descargas raw (`dirty`, `clean`, `ground truth`) no resolvieron el host en esta corrida. |
| AddressTable | No medido | [Registro AddressTable](https://zenodo.org/records/20841898); `ADDRESSTABLE_DATA_DIR` para una muestra local o `ADDRESSTABLE_FILE_URL` para una URL directa; `ADDRESSTABLE_MAX_ROWS` declara la muestra leida. La licencia de datos no pudo confirmarse desde el registro indicado; no se redistribuye. | El registro es de escala de varios GB y no expone en la nota una URL directa de muestra; el script evita descargar el archivo completo sin una copia y permiso concretos. |

Los conteos de filas, columnas o tamano que la nota de desarrollo atribuye a
estas fuentes se mantienen como contexto de la fuente, no como resultados
medidos por este banco. La unica medicion adicional publicada en esta corrida
es, por tanto, la reproduccion de Raha anterior.

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
