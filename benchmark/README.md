# Benchmark reproducible con los pares de Raha

Esta carpeta publica la evidencia detrás de la fila «Raha dirty/clean pairs»
del README principal. No forma parte del paquete instalado: está excluida del
tarball mediante `.Rbuildignore`. Los CSV de Raha no se redistribuyen; se descargan del
repositorio de [Raha](https://github.com/BigDaMa/raha) durante cada corrida.

## Datos para las figuras

Las cuatro figuras de rendimiento del README principal (`man/figures/banco-*.png`)
se rehacen desde esta carpeta en tres pasos, y ninguna cifra que muestran está
escrita en un guion: todas salen de los CSV de `benchmark/datos/`.

1. `medir_figuras.R` mide la instalación de `lupa` visible en `.libPaths()`
   —imprime su versión, su sello `Built` y su ruta, y se detiene si falta el
   sello o la capacidad opcional `stringdist`— y deja un CSV por tabla:
   - `entorno.csv` identifica la corrida: fecha, versión, `Built`, R, CPU,
     núcleos, RAM, hilos de `stringdist`, y la medida final con su umbral y su
     `p`, leídos de los valores por omisión de la instalación medida;
   - `escala.csv`: por tamaño, candidatos del tamiz, candidatos previstos por
     la estimación previa y su error, pares informados, recall respecto del
     techo sembrado, las tres corridas de tiempo y su mediana, y el RSS máximo
     del proceso (`VmHWM`; `NA` donde no hay `/proc`). Cada corrida es un
     proceso R aparte, para que el RSS sea de esa corrida;
   - `hilos.csv`: lo mismo a 100.000 filas para 2, 4, 8, 16 y
     `detectCores() - 1` hilos. Se omite con `LUPA_FIGURAS_SIN_HILOS=1`;
   - `cardinalidad.csv`, `bloqueo.csv`, `presupuesto.csv` y `loteo.csv`: las
     tablas de las secciones 3 a 6 del guion.
2. `perdida_lsh.R` deja `perdida_lsh_bandas.csv` y `perdida_lsh_umbral.csv`,
   una fila por configuración cruda; el diseño está en
   [`perdida_lsh.md`](perdida_lsh.md).
3. `graficar_figuras.R` lee los CSV y dibuja los cuatro PNG con R base, sin
   dependencias gráficas: `Rscript benchmark/graficar_figuras.R [dir_datos]
   [dir_salida]`. Se detiene si los CSV mezclan commits o si falta una columna,
   y al final imprime las cifras que el README cita en prosa, para que otra
   corrida diga con un `diff` qué frases quedaron viejas.
   También se detiene ante un CSV que lo haría **afirmar lo que el dato no
   dice**: un valor no finito en una columna que se dibuja —antes el título
   salía «entre NA % y NA %»—, una proporción fuera de `[0, 1]` —«150 %»—, más
   de una fila en `entorno.csv` o en `bloqueo.csv` —dibujaba la primera y
   callaba el resto—, `hilos.csv` con tamaños distintos —el panel titula con
   uno solo— o repetidos, y un nivel de cardinalidad que no sabe describir
   —dibujaba una fila rotulada «NA»—. Cada mensaje nombra el archivo, la
   columna y la fila. Ninguno de esos casos sale de una corrida sana del
   medidor: salieron de darle CSV rotos a propósito, de a uno.

Todos los CSV llevan el commit como última columna. La firma está en
[`_firma.R`](_firma.R), que usan los dos medidores, y promete exactamente tres
cosas:

- el commit es el `HEAD` del árbol desde el que se corrió;
- se marca `+sucio` si tienen cambios sin commitear las rutas que determinan el
  resultado: `R/`, `DESCRIPTION`, `NAMESPACE`, `src/`, `inst/` **y el propio
  guion que mide** —`medir_figuras.R` con `_padron_sintetico.R`, o
  `perdida_lsh.R`—. Que el medidor quedara afuera de esa lista fue un hueco
  real: un CSV podía firmarse con un commit teniendo modificado, o sin
  versionar, el guion que lo produjo. `datos/` queda afuera a propósito, porque
  lo escribe la propia corrida y marcaría siempre;
- antes de medir, el guion se **detiene** si el sello `Built` de la instalación
  es anterior al último commit de esas rutas: esa instalación no puede contener
  el código que se firmaría. Es condición necesaria y no suficiente —un armado
  posterior puede venir de otro árbol—; atrapa la instalación vieja olvidada en
  la biblioteca por omisión, que es el caso frecuente, no a quien quiera
  engañarla. Para medir igual, a sabiendas, `LUPA_FIGURAS_SIN_GUARDA_BUILT=1`
  deja pasar y firma los CSV con el sufijo `+singuarda`, para que quede en el
  dato y no sólo en la consola.

`entorno.csv` publica además `instalacion`, la biblioteca de la que salió lo
que se midió, con el hogar abreviado a `~`.

**Procedencia de los CSV que hay hoy en `datos/`**, dicha sin redondear: salen
de la corrida del 2026-09-02 sobre `5645c15` —la instalación que midió esa
corrida se armó desde ese commit, comprobado comparando su `NAMESPACE` con el
del árbol—, y son **anteriores a los cambios de esta vuelta en el medidor**: la
guarda del `Built`, la firma del propio guion, la columna `instalacion` y la
comprobación de la estimación previa entre las tres corridas. Ninguno de esos
cambios mueve una cifra medida —el primero detiene o no detiene, los otros
agregan una columna o una comprobación—, pero por eso los CSV llevan una firma
limpia que el firmador de hoy no habría emitido: en aquel momento el guion que
medía no estaba versionado y la lista de rutas firmadas no lo incluía. La
próxima corrida completa los reemplaza y ya nace con la firma nueva.

`datos/` se versiona porque las figuras se construyen desde ahí y no desde una
transcripción de consola.

Desde la raíz del repositorio, con la máquina quieta:

```sh
R CMD build . && R CMD INSTALL lupa_0.1.0.tar.gz
Rscript benchmark/medir_figuras.R
Rscript benchmark/perdida_lsh.R
Rscript benchmark/graficar_figuras.R
```

`medir_figuras.R` mide 20.000, 100.000, 200.000 y 500.000 filas; 1.000.000 se
agrega sólo pidiéndolo (`Rscript benchmark/medir_figuras.R 1000000`). Para
comprobar el banco sin las partes caras, `LUPA_FIGURAS_SOLO_BARATAS=1`
conserva el caso de 20.000 filas y las tablas de cardinalidad, bloqueo,
presupuesto y loteo, y saltea los tamaños mayores y la tabla de hilos.

### Tiempos de referencia

Medidos el 2026-09-02 sobre `5645c15`, con dos hilos de `stringdist`, en la
máquina que describe `entorno.csv`. Son órdenes de magnitud para planificar,
no tiempos universales.

```
prueba de humo (LUPA_FIGURAS_SOLO_BARATAS=1)                130 s
corrida completa de medir_figuras.R                    3 h 34 min
  escala, 20.000 filas, mediana de tres procesos         15,8 s
  escala, 100.000 filas                                 156,1 s
  escala, 200.000 filas                                 582,9 s
  escala, 500.000 filas                               2.937,4 s
  hilos, cinco configuraciones por tres procesos         27 min
perdida_lsh.R, las dos tablas                              22 s
graficar_figuras.R                                        < 1 s
```

Cada tamaño de escala corre tres procesos, así que la sección pesa el triple
de la mediana que muestra la tabla; la corrida completa va de las 11:28:51 a
las 15:02:31 del registro.

## Qué se mide

`verdad_raha.R` lee `dirty.csv` y `clean.csv` como texto, conserva las cadenas
vacías y compara sus matrices celda a celda. Las unidades son:

- **celdas diferentes**: posiciones cuyo texto difiere entre ambas matrices;
- **filas afectadas**: filas con al menos una de esas posiciones;
- **tasa**: celdas diferentes divididas por todas las celdas de la tabla;
- **columnas afectadas**: columnas con al menos una celda diferente;
- **columnas señaladas por lupa**: columnas con al menos un hallazgo de
  severidad `sospechoso` o `error`.

La comparación de Raha etiqueta **celdas cambiadas**; la evaluación de `lupa`
es **por columna**. Que una columna afectada reciba un hallazgo no demuestra que
`lupa` haya identificado cada cambio, ni siquiera el mismo tipo de problema.
Por eso 26/26 es cobertura de columnas afectadas, no recall diagnóstico.

Las ocho columnas señaladas adicionales fueron revisadas manualmente. En todas
hay una observación apoyada por los datos que la comparación dirty/clean no
pretende etiquetar: constantes, columnas duplicadas, mayúsculas inconsistentes,
cinco cadenas vacías y texto de alta cardinalidad. No se las presenta como
falsos positivos ni se calcula precisión a partir de estos pares.

## Cómo correrlo

Se necesita R, conexión a internet y una instalación de `lupa` con sus
dependencias sugeridas. Los números publicados sólo se reproducen contra una
instalación construida desde estas mismas fuentes. La versión `0.1.0` por sí
sola no alcanza para identificarla: `medir_lupa.R` imprime la versión y el sello
`Built` completo de la instalación medida, y se detiene si no encuentra las
capacidades de las que depende la tabla. En particular, las variantes de
vocabulario breve de Hospital requieren la capacidad opcional `stringdist`;
el script se detiene en vez de publicar una cobertura parcial si no está
instalada. Desde la raíz del repositorio:

```sh
R CMD build . && R CMD INSTALL lupa_0.1.0.tar.gz
Rscript benchmark/verdad_raha.R
Rscript benchmark/medir_lupa.R
```

Los scripts sólo cargan R base, `utils` y `lupa`; no adjuntan `stringdist` ni
requieren `readr`, `dplyr` o `tibble`. `lupa` usa internamente aquella capacidad
opcional durante el perfilado. Por omisión descargan los seis archivos de
`https://raw.githubusercontent.com/BigDaMa/raha/master/datasets/<ds>/<dirty|clean>.csv`
a un directorio temporal. Para repetir la corrida sin red sobre una copia ya
obtenida se puede definir `RAHA_DATA_DIR`; sus archivos deben llamarse, por
ejemplo, `hospital_dirty.csv` y `hospital_clean.csv`.

Cada salida registra URL, tamaño y una huella Adler-32 de los bytes obtenidos.
La huella no es criptográfica: sirve como identificador reproducible para
detectar que el archivo de origen cambió. El snapshot publicado está en
[`resultados.md`](resultados.md).

## Interpretación

Cada comprobación del README principal declara su propia unidad y referencia;
ninguna estima una exactitud única para todo el paquete. Esta batería tampoco
compara conteos de celdas de `lupa` con los de Raha. Por ejemplo, una columna
puede estar afectada porque cambió la representación de una unidad y recibir un
hallazgo verdadero sobre esa representación sin que exista una correspondencia
uno a uno entre celdas.

## Bancos adicionales

`medir_topes_valores_ancho.R` mide la discriminacion de Jaro-Winkler en pares
de textos que difieren en un caracter y en 1.000, y cronometra el perfilado de
500 filas con 50, 300 y 1.000 columnas. Ejecutarlo despues de instalar la
version construida desde estas fuentes:

```sh
Rscript benchmark/medir_topes_valores_ancho.R
```

El script imprime la mediana de cinco semillas para la tabla de discriminacion
y deja el aviso de tabla ancha desactivado durante el cronometraje.

Los scripts siguientes son independientes de `verdad_raha.R` y no agregan
dependencias al paquete:

```sh
Rscript benchmark/verdad_ped.R
Rscript benchmark/verdad_tableeg.R
Rscript benchmark/verdad_riolu.R
Rscript benchmark/verdad_addresstable.R
Rscript benchmark/medir_bancos.R
```

Cada script imprime `no medido` cuando no puede obtener una copia completa y
alineada. Esa salida no se convierte en cero. Las variables de entorno para
repetir una corrida sin red son `PED_DATA_DIR`, `TABLEEG_DATA_DIR`,
`RIOLU_DATA_DIR` y `ADDRESSTABLE_DATA_DIR`. Las cuatro deben apuntar a una
copia local ya extraida; los CSV no se versionan.

PED se obtiene del repositorio de `twinklelittlestars/PED`. No se encontro una
licencia de datos declarada en el repositorio; por eso se descarga para medir y
no se redistribuye. Por defecto se intentan `Flight` y `Hospital`; se puede
cambiar la lista con `PED_DATASETS`. La verdad publicada es `difference.csv`,
que usa las columnas `Index` y `Attribute`; `dirty.csv` y `clean.csv` se
comparan en R base sólo como comprobacion de alineacion. Las rutas remotas son
`data/<Conjunto>/<archivo>` en la rama `main`.

TableEG describe sus datos en una carpeta de Google Drive, no en una URL de
archivo directa. El script consulta esa carpeta como HTML, pero no trata la
pagina como si fuera un ZIP ni intenta descargar a ciegas todo su contenido.
El repositorio tampoco declara una licencia de datos. Se requiere
`TABLEEG_DATA_DIR` o una URL directa de un archivo ZIP en
`TABLEEG_ARCHIVE_URL`; el script puede extraer ese ZIP con `utils::unzip` y no
lo versiona. Las anotaciones JSONL se registran como metadato si estan en la
copia, mientras que la verdad de celdas se valida con el par alineado.

RIOLU tiene licencia MIT para el codigo de la replicacion, pero no declara una
licencia separada para los datos reutilizados. El script usa sus archivos
`dirty_*` y `clean_*` desde `test_anomaly_detection/<dataset>/`, y `gt_*.csv`
desde `ground_truth_anomaly_detection/`, desde GitHub o `RIOLU_DATA_DIR`.
Por defecto intenta `flights`, `hosp_100k`, `hosp_10k`, `hosp_1k` y `movies`;
se puede cambiar la lista con `RIOLU_DATASETS`. La verdad de patrones es
exactamente la etiqueta `1`; `-1` es nulo y `0` es normal.

AddressTable se consulta en Zenodo: el script intenta la metadata de la API y
del registro web. El archivo completo reportado por la fuente es de escala de
varios GB, por lo que no se descarga implicitamente: requiere una muestra local
o dos URLs autorizadas, `ADDRESSTABLE_FILE_URL` y
`ADDRESSTABLE_CLEAN_URL`, y limita la lectura a `ADDRESSTABLE_MAX_ROWS` filas.
La licencia aplicable a los datos no pudo confirmarse desde el registro
indicado; no se redistribuye ninguna copia.

La medicion de los bancos de errores por celda usa dos nombres que no se
deben confundir: `celdas_verdad` cuenta las posiciones con diferencia o etiqueta
de error; `celdas_con_hallazgo_trazable` cuenta posiciones que `lupa` adjunta a
un hallazgo no `ok`. La interseccion se publica como precision de celdas
trazables y cobertura de celdas de verdad, no como recall diagnostico. En
AddressTable ambos numeros se refieren a la muestra declarada.
