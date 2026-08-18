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

## PED y RIOLU: estado de disponibilidad y alcance

No se inventa un numero cuando no hubo una copia alineada. En esta corrida se
intentaron las URLs exactas de raw GitHub; el entorno de ejecucion no pudo
resolver `raw.githubusercontent.com`. Esto es una limitacion local de red, no
un diagnostico de que los hosts remotos esten caidos. Los scripts tambien fueron
probados con copias locales y entonces construyen la verdad y pasan el resultado
a `lupa`; se pueden repetir sin red con `PED_DATA_DIR` y `RIOLU_DATA_DIR`.

La unidad de `PED` es la posicion `(fila, columna)` listada en
`difference.csv` (`Index`, `Attribute`). La unidad de `RIOLU` es la posicion
cuya etiqueta en `gt_*.csv` es exactamente `1`; `0` y `-1` no cuentan. En ambos
casos `dirty.csv` y `clean.csv` solo comprueban que la copia este alineada: no
reemplazan la verdad publicada.

| Banco / conjunto | Estado | Numero que se publicaria si la copia estuviera disponible | Condiciones de descarga y licencia |
| --- | --- | --- | --- |
| PED / Flight | No medido en esta corrida | Sin numero: no hubo copia local ni descarga ejecutable | [Raw PED](https://raw.githubusercontent.com/twinklelittlestars/PED/main/data/Flight/); `PED_DATA_DIR`. El repositorio no declara licencia; se baja para medir y no se redistribuye. |
| PED / Hospital | No medido en esta corrida | Sin numero: no hubo copia local ni descarga ejecutable | [Raw PED](https://raw.githubusercontent.com/twinklelittlestars/PED/main/data/Hospital/); `PED_DATA_DIR`. Sin licencia declarada; no se redistribuye. |
| PED / MIMIC | No medido en esta corrida | Sin numero: no hubo copia local ni descarga ejecutable | [Raw PED](https://raw.githubusercontent.com/twinklelittlestars/PED/main/data/MIMIC/); `PED_DATA_DIR`. Sin licencia declarada; no se redistribuye. |
| PED / Plane | No medido en esta corrida | Sin numero: no hubo copia local ni descarga ejecutable | [Raw PED](https://raw.githubusercontent.com/twinklelittlestars/PED/main/data/Plane/); `PED_DATA_DIR`. Sin licencia declarada; no se redistribuye. |
| PED / Soccer | No medido en esta corrida | Sin numero: no hubo copia local ni descarga ejecutable | [Raw PED](https://raw.githubusercontent.com/twinklelittlestars/PED/main/data/Soccer/); `PED_DATA_DIR`. Sin licencia declarada; no se redistribuye. |
| RIOLU / flights | No medido en esta corrida | Sin numero: no hubo copia local ni descarga ejecutable | [Raw RIOLU](https://raw.githubusercontent.com/mooselab/Discover-Data-Quality-With-RIOLU/main/); codigo MIT; licencia separada de los datos no declarada; no se redistribuyen. |
| RIOLU / hosp_100k | No medido en esta corrida | Sin numero: no hubo copia local ni descarga ejecutable | Misma fuente y condiciones que RIOLU; `RIOLU_DATA_DIR`. |
| RIOLU / hosp_10k | No medido en esta corrida | Sin numero: no hubo copia local ni descarga ejecutable | Misma fuente y condiciones que RIOLU; `RIOLU_DATA_DIR`. |
| RIOLU / hosp_1k | No medido en esta corrida | Sin numero: no hubo copia local ni descarga ejecutable | Misma fuente y condiciones que RIOLU; `RIOLU_DATA_DIR`. |
| RIOLU / movies | No medido en esta corrida | Sin numero: no hubo copia local ni descarga ejecutable | Misma fuente y condiciones que RIOLU; `RIOLU_DATA_DIR`. |

Cuando haya datos, `benchmark/medir_bancos.R` publicara por cada conjunto:

- `celdas_verdad`: posiciones de la verdad del banco;
- `celdas_con_hallazgo_trazable`: union de posiciones que `lupa` adjunta a
  hallazgos no `ok`, con filas localizables;
- `celdas_acertadas_trazables`: interseccion de las dos anteriores;
- `precision_celdas_trazables`: aciertos trazables dividido por hallazgos
  trazables;
- `cobertura_celdas_verdad`: aciertos trazables dividido por celdas de verdad;
- `columnas_verdad_con_hallazgo`: interseccion de columnas con verdad y
  columnas con hallazgo.

Estos nombres declaran los denominadores. `cobertura_celdas_verdad` no es
recall de un clasificador y `columnas_verdad_con_hallazgo` es cobertura de
columnas, no recall de celdas.

Como control de procedencia, los tamanos de bytes informados para las fuentes
son metadatos del archivo, no resultados de `lupa`: PED Flight tiene
`clean.csv` de 159992, `dirty.csv` de 163038 y `difference.csv` de 41526;
PED Hospital tiene 299425, 299433 y 7359 respectivamente; RIOLU informa
`gt_flights.csv` 1556500, `gt_hosp_100k.csv` 1288912,
`gt_hosp_10k.csv` 118911, `gt_hosp_1k.csv` 10900 y `gt_movies.csv` 103021.

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
