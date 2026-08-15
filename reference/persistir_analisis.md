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

## Details

La protección de datos personales con evidencia suficiente se vuelve a
aplicar antes de escribir. Incluir datos que contienen columnas
protegidas exige desactivar expresamente esa protección; una
clasificación débil se conserva como información pero no activa esa
restricción.

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
