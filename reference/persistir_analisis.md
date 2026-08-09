# Guardar y recuperar un análisis

Persiste un objeto `analizar()` en RDS con número de esquema. Los datos
de entrada no se guardan por omisión. Las funciones de reglas se
sustituyen por declaraciones pequeñas: una dependencia funcional se
reconstruye al leer sólo si los datos fueron incluidos; una función
arbitraria queda desactivada. Así el archivo no serializa entornos de
ejecución completos.

## Uso

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

## Argumentos

  - x:
    
    Objeto creado por `analizar()`.

  - archivo:
    
    Ruta del archivo RDS.

  - incluir\_datos:
    
    Si se persiste la copia de los datos conservada por
    `analizar(conservar_datos = TRUE)`.

  - proteger\_datos\_personales:
    
    Si se protege toda evidencia derivada.

  - sobrescribir:
    
    Si se permite reemplazar un archivo existente.

  - comprimir:
    
    Compresión admitida por `saveRDS()`: un lógico, `"gzip"`, `"bzip2"`
    o `"xz"`.

## Valor

`guardar_analisis()` devuelve la ruta de forma invisible;
`leer_analisis()` devuelve un objeto `analisis`.

## Detalles

La protección de datos personales con evidencia suficiente se vuelve a
aplicar antes de escribir. Incluir datos que contienen columnas
protegidas exige desactivar expresamente esa protección; una
clasificación débil se conserva como información pero no activa esa
restricción.

## Ver también

`analizar()`, `reportar()`, `guardar_historico()`

## Ejemplos

``` r
a <- analizar(datos_administrativos, argumentos_perfil = list(
  analizar_dependencias = FALSE
))
ruta <- tempfile(fileext = ".rds")
guardar_analisis(a, ruta)
b <- leer_analisis(ruta)
unlink(ruta)
```
