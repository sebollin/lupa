# Guardar y recuperar un análisis

Persiste un objeto
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md) en
RDS con número de esquema. Los datos de entrada no se guardan por
omisión. Las funciones de reglas se sustituyen por declaraciones
pequeñas: una dependencia funcional se reconstruye al leer sólo si los
datos fueron incluidos; una función arbitraria queda desactivada. Así el
archivo no serializa entornos de ejecución completos.

## Usage

``` r
guardar_analisis(
  x,
  archivo,
  incluir_datos = FALSE,
  proteger_datos_personales = TRUE,
  sobrescribir = FALSE,
  comprimir = "xz"
)

leer_analisis(archivo)
```

## Arguments

- x:

  Objeto creado por
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md).

- archivo:

  Ruta del archivo RDS.

- incluir_datos:

  Si se persiste la copia de los datos conservada por
  `analizar(conservar_datos = TRUE)`.

- proteger_datos_personales:

  Si se protege toda evidencia derivada.

- sobrescribir:

  Si se permite reemplazar un archivo existente.

- comprimir:

  Compresión admitida por
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html): un lógico,
  `"gzip"`, `"bzip2"` o `"xz"`.

## Value

`guardar_analisis()` devuelve la ruta de forma invisible;
`leer_analisis()` devuelve un objeto `analisis`.

El objeto guardado gana `meta$persistencia`, que declara qué se guardó y
qué no: `version_esquema`, si los datos viajan (`datos_incluidos`), si
la evidencia salió protegida (`evidencia_protegida`) y cuántas funciones
se sustituyeron por su descripción (`funciones_sustituidas`). Es el
único campo que un objeto leído tiene y el original no: lo que se guarda
no es idéntico a lo que se analizó, y esa diferencia se declara en vez
de suponerse.

## Details

La protección de datos personales con evidencia suficiente se vuelve a
aplicar antes de escribir. Incluir datos que contienen columnas
protegidas exige desactivar expresamente esa protección; una
clasificación débil se conserva como información pero no activa esa
restricción.

El archivo se escribe con el **formato RDS 2**, no con el 3 que
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) usa por omisión. El 3
anota la codificación nativa de quien escribe y al leer *traduce* desde
ella el texto que no declara la suya; en Windows esa traducción no falla
y cambia los bytes en silencio, así que un análisis guardado bajo un
locale y leído bajo otro podía volver con el texto alterado.

Ese formato tiene un costo y conviene saberlo: **no conserva la
representación compacta** de los vectores que R guarda así. Medido, una
columna `1:1e6` ocupa 103 bytes con el formato 3 y 2.127.903 con el 2;
con `incluir_datos = TRUE` sobre una tabla con una columna
`id = seq_len(n)`, eso son unos 2 MB por cada millón de filas, y a
escala la escritura puede exigir materializar en memoria lo que el
formato 3 no materializa. Se eligió así porque un archivo más grande es
visible y recuperable, y un texto corrompido en silencio no lo es. Quien
prefiera la otra relación puede serializar el objeto por su cuenta.

## See also

[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md),
[`guardar_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md)

## Examples

``` r
a <- analizar(datos_administrativos, argumentos_perfil = list(
  analizar_dependencias = FALSE
))
ruta <- tempfile(fileext = ".rds")
guardar_analisis(a, ruta)
b <- leer_analisis(ruta)
unlink(ruta)
```
