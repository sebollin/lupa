# Crear un reporte HTML autocontenido

Genera un unico archivo HTML en espanol usando solo funciones de R base.
El estilo se incluye dentro del documento: no requiere red, navegador
especial, conversor externo ni archivos auxiliares. Cada valor dinamico
se escapa antes de incorporarlo al documento.

## Uso

``` r
reportar(
  x,
  ...,
  archivo = tempfile("reporte-lupa-", fileext = ".html"),
  sobrescribir = FALSE,
  titulo = "Reporte de calidad de datos",
  fecha = Sys.time(),
  max_filas = 100L,
  max_patrones = 20L,
  proteger_datos_personales = TRUE
)
```

## Argumentos

  - x:
    
    Un objeto compatible o una lista de objetos compatibles.

  - ...:
    
    Objetos adicionales de clase `analisis`, `perfil`, `medicion`,
    `evaluacion_calidad`, `historico_calidad`, `deriva_perfil`,
    `deriva_calidad`, `plan_limpieza` o `duplicados_aproximados`.

  - archivo:
    
    Ruta de salida. De forma predeterminada crea un archivo en
    `tempdir()`.

  - sobrescribir:
    
    Si se permite reemplazar un archivo existente.

  - titulo:
    
    Titulo visible del reporte.

  - fecha:
    
    Fecha y hora de generacion, inyectable para obtener resultados
    reproducibles. Se normaliza a UTC.

  - max\_filas:
    
    Maximo de filas por tabla y de columnas del perfil cuyos patrones se
    detallan. Las omisiones se informan dentro del reporte.

  - max\_patrones:
    
    Maximo de patrones mostrados por columna. Las omisiones se informan
    dentro del reporte.

  - proteger\_datos\_personales:
    
    Si se enmascaran modas, ejemplos, evidencia, estadisticos de orden,
    cuantiles y rangos temporales de columnas cuya clasificacion activa
    proteccion automatica. Es `TRUE` por defecto. Las coincidencias
    debiles se informan sin suprimir. Para ver valores concretos deben
    haberse conservado tambien con `perfilar(...,
    proteger_datos_personales = FALSE)`.

## Valor

La ruta normalizada del archivo, de forma invisible.

## Detalles

Se pueden combinar objetos producidos por el profiling, la medicion, la
evaluacion, el historico, las comparaciones de deriva y la planificacion
de limpieza. Cada tipo anade su seccion; el reporte no modifica datos ni
aplica planes.

## Ver también

`perfilar()`, `medir()`, `evaluar()`, `historico_calidad()`,
`comparar_perfiles()`, `planificar_limpieza()`

## Ejemplos

``` r
perfil <- perfilar(datos_administrativos)
archivo <- reportar(perfil)
unlink(archivo)
```
