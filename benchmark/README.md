# Benchmark reproducible con los pares de Raha

Esta carpeta publica la evidencia detrás de la fila «Raha dirty/clean pairs»
del README principal. No forma parte del paquete instalado: está excluida del
tarball mediante `.Rbuildignore`. Los CSV no se redistribuyen; se descargan del
repositorio de [Raha](https://github.com/BigDaMa/raha) durante cada corrida.

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
