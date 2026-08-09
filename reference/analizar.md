# Ejecutar el análisis descriptivo completo

Es la puerta de entrada al recorrido de `lupa`. Construye el perfil, las
distribuciones, asociaciones, diagnóstico temporal, clasificación
confirmable de variables, propuesta de modelo, cobertura conceptual y
plan de limpieza. No modifica datos ni mide la propuesta generada
automáticamente.

## Uso

``` r
analizar(
  datos,
  nombre = deparse(substitute(datos)),
  fecha = Sys.time(),
  argumentos_perfil = list(),
  metadatos_variables = NULL,
  modelo_confirmado = NULL,
  propuesta_confirmada = NULL,
  marco = NULL,
  perfil_evaluacion = NULL,
  id_medicion = NULL,
  muestra = 1e+05,
  muestra_asociacion = 10000,
  max_valores = 20L,
  probabilidades = c(0, 0.25, 0.5, 0.75, 1),
  umbral_asociacion = 0.3,
  max_columnas_asociacion = 50L,
  max_niveles_asociacion = 50L,
  max_pares_asociacion = 500L,
  calendario = 1:7,
  frecuencia_dias = NULL,
  max_huecos = 20L,
  max_columnas_temporales = 50L,
  conservar_datos = FALSE,
  proteger_datos_personales = TRUE,
  ...
)
```

## Argumentos

  - datos:
    
    Tabla que se desea analizar.

  - nombre:
    
    Nombre de la entrega.

  - fecha:
    
    Fecha y hora reproducible de la corrida.

  - argumentos\_perfil:
    
    Lista de argumentos adicionales para `perfilar()`.

  - metadatos\_variables:
    
    Declaraciones para `clasificar_variables()`.

  - modelo\_confirmado:
    
    Modelo creado por `modelo()` o `NULL`.

  - propuesta\_confirmada:
    
    Propuesta editada por el usuario o `NULL`.

  - marco:
    
    Taxonomía opcional creada por `marco_calidad()`. Si se omite, usa la
    asociada a `modelo_confirmado` y, en último término,
    `marco_agesic()`.

  - perfil\_evaluacion:
    
    Perfil explícito para `evaluar()` o `NULL`.

  - id\_medicion:
    
    Identificador opcional enviado a `medir()`.

  - muestra:
    
    Límite de filas para perfil, distribuciones y enumeración de niveles
    observados.

  - muestra\_asociacion:
    
    Límite común de filas para asociaciones.

  - max\_valores:
    
    Máximo de valores por tabla de frecuencias.

  - probabilidades:
    
    Cuantiles solicitados.

  - umbral\_asociacion:
    
    Asociación mínima informada.

  - max\_columnas\_asociacion:
    
    Máximo de columnas para asociaciones.

  - max\_niveles\_asociacion:
    
    Máximo de niveles categóricos.

  - max\_pares\_asociacion:
    
    Máximo de pares devueltos.

  - calendario:
    
    Días ISO usados por el análisis temporal.

  - frecuencia\_dias:
    
    Frecuencia temporal conocida o `NULL` para proponerla.

  - max\_huecos:
    
    Máximo de grupos de huecos por columna.

  - max\_columnas\_temporales:
    
    Máximo de columnas temporales analizadas.

  - conservar\_datos:
    
    Si el objeto retiene una copia de la entrada. Es `FALSE` por omisión
    para limitar tamaño y exposición. Con protección activa, las
    columnas personales de esa copia también se enmascaran; para
    conservar sus valores debe usarse `proteger_datos_personales =
    FALSE`.

  - proteger\_datos\_personales:
    
    Si perfiles y resúmenes ocultan valores de columnas cuya
    clasificación activa protección automática, incluidos estadísticos
    de orden, cuantiles y rangos temporales.

  - ...:
    
    Argumentos con nombre enviados a `perfilar()`. Es una alternativa
    concisa a `argumentos_perfil`.

## Valor

Objeto S3 `analisis` con todos los componentes y su cobertura.

## Detalles

La cadena de medición sólo se completa si se recibe `modelo_confirmado`
o `propuesta_confirmada`. En el segundo caso se materializan únicamente
sus filas activas mediante `modelo_desde_propuesta()`. La evaluación
requiere además un `perfil_evaluacion` explícito.

## Ver también

`guardar_analisis()`, `reportar()`, `cobertura_analisis()`

## Ejemplos

``` r
resultado <- analizar(
  datos_administrativos, analizar_dependencias = FALSE,
  fecha = as.POSIXct("2026-01-15", tz = "UTC")
)
resultado
#> 
#> ── Analisis de datos: datos_administrativos ────────────────────────────────────
#> Filas: 13
#> Columnas: 10
#> Hallazgos del perfil: 19
#> Advertencias de alcance: 2
#> Asociaciones informadas: 4
#> Series temporales: 1
#> Modelo medido: no
#> 
#> ── Cobertura conceptual ──
#> 
#>            estado factores
#>            medida        2
#>      no_declarada       10
#>         no_aplica        4
#>  fuera_de_alcance        1
#> ── Advertencias de alcance ──
#> 
#>  componente                     tipo
#>      tiempo frecuencia_no_confirmada
#>   variables   escalas_no_confirmadas
#>                                                                       descripcion
#>  Las frecuencias temporales son propuestas observadas, no requisitos confirmados.
#>        Algunas escalas se propusieron desde los valores y requieren confirmacion.
```
