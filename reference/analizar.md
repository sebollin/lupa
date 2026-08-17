# Ejecutar el análisis descriptivo completo

Es la puerta de entrada al recorrido de `lupa`. Construye el perfil, las
distribuciones, asociaciones, diagnóstico temporal, clasificación
confirmable de variables, propuesta de modelo, cobertura conceptual y
plan de limpieza. No modifica datos ni mide la propuesta generada
automáticamente.

## Usage

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

## Arguments

- datos:

  Tabla que se desea analizar.

- nombre:

  Nombre de la entrega.

- fecha:

  Fecha y hora reproducible de la corrida.

- argumentos_perfil:

  Lista de argumentos adicionales para
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

- metadatos_variables:

  Declaraciones para
  [`clasificar_variables()`](https://sebollin.github.io/lupa/reference/clasificar_variables.md).

- modelo_confirmado:

  Modelo creado por
  [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  o `NULL`.

- propuesta_confirmada:

  Propuesta editada por el usuario o `NULL`.

- marco:

  Taxonomía opcional creada por
  [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md).
  Si se omite, usa la asociada a `modelo_confirmado` y, en último
  término,
  [`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md).

- perfil_evaluacion:

  Perfil explícito para
  [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md) o
  `NULL`.

- id_medicion:

  Identificador opcional enviado a
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md).

- muestra:

  Límite de filas para perfil, distribuciones y enumeración de niveles
  observados.

- muestra_asociacion:

  Límite común de filas para asociaciones.

- max_valores:

  Máximo de valores por tabla de frecuencias.

- probabilidades:

  Cuantiles solicitados.

- umbral_asociacion:

  Asociación mínima informada.

- max_columnas_asociacion:

  Máximo de columnas para asociaciones.

- max_niveles_asociacion:

  Máximo de niveles categóricos.

- max_pares_asociacion:

  Máximo de pares devueltos.

- calendario:

  Días ISO usados por el análisis temporal.

- frecuencia_dias:

  Frecuencia temporal conocida o `NULL` para proponerla.

- max_huecos:

  Máximo de grupos de huecos por columna.

- max_columnas_temporales:

  Máximo de columnas temporales analizadas.

- conservar_datos:

  Si el objeto retiene una copia de la entrada. Es `FALSE` por omisión
  para limitar tamaño y exposición. Con protección activa, las columnas
  personales de esa copia también se enmascaran; para conservar sus
  valores debe usarse `proteger_datos_personales = FALSE`.

- proteger_datos_personales:

  Si perfiles y resúmenes ocultan valores de columnas cuya clasificación
  activa protección automática, incluidos estadísticos de orden,
  cuantiles y rangos temporales.

- ...:

  Argumentos con nombre enviados a
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
  Es una alternativa concisa a `argumentos_perfil`.

## Value

Objeto S3 `analisis` con todos los componentes y su cobertura.

## Details

La cadena de medición sólo se completa si se recibe `modelo_confirmado`
o `propuesta_confirmada`. En el segundo caso se materializan únicamente
sus filas activas mediante
[`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md).
La evaluación requiere además un `perfil_evaluacion` explícito.

## See also

[`guardar_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md),
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md),
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)

## Examples

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
#> Hallazgos del perfil: 20
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
