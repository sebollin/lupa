# Ejecutar el análisis descriptivo completo

Es la puerta de entrada al recorrido de `lupa`. Construye el perfil, las
distribuciones, asociaciones, diagnóstico temporal, clasificación
confirmable de variables, propuesta de modelo, medición agregada,
tablero, cobertura conceptual y plan de limpieza. No modifica los datos.

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
  medir_propuesta = TRUE,
  conservar_detalle_medicion = FALSE,
  muestra = 1e+05,
  muestra_asociacion = 10000,
  max_valores = 20L,
  probabilidades = c(0, 0.25, 0.5, 0.75, 1),
  umbral_asociacion = 0.3,
  max_columnas_asociacion = 50L,
  max_niveles_asociacion = 50L,
  max_pares_asociacion = 500L,
  metodo_asociacion_numerica = c("pearson", "spearman"),
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

- medir_propuesta:

  Si se mide automáticamente la propuesta en estado `"lista"` cuando no
  se recibe un modelo o una propuesta confirmada. Use `FALSE` para
  conservar el comportamiento descriptivo anterior.

- conservar_detalle_medicion:

  Si se retienen las medidas fila a fila. Es `FALSE` por omisión: el
  tablero y la medición agregada permanecen.

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

- metodo_asociacion_numerica:

  Medida entre columnas numéricas que usa
  [`detectar_asociaciones()`](https://sebollin.github.io/lupa/reference/detectar_asociaciones.md):
  `"pearson"` por omisión, o `"spearman"` para asociación monótona sobre
  los rangos, que no supone linealidad.

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

Por omisión mide todas las sugerencias de la propuesta cuyo estado es
`"lista"`, aunque no estuvieran activas, y declara que esa selección fue
realizada por `lupa` sin confirmación. Agrega inmediatamente el detalle
y conserva
[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md);
las medidas fila a fila sólo quedan en el resultado si
`conservar_detalle_medicion = TRUE`. La evaluación, cuando se solicita,
usa la medición agregada. Los hallazgos `casi_clave` del perfil se
reiteran en `advertencias`, con su columna, valores en colisión,
frecuencias y criterio observado, para que no queden ocultos dentro del
recorrido integral.

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
#> Hallazgos del perfil: 23
#> Advertencias de alcance: 2
#> Asociaciones informadas: 4
#> Series temporales: 1
#> Modelo medido: si
#> Detalle fila a fila: no conservado
#> 
#> ── Tablero de calidad ──
#> 
#> ── Tablero de calidad ──────────────────────────────────────────────────────────
#>       componente    dimension                 factor
#>  componente-0001     Unicidad         No-duplicación
#>  componente-0002  Completitud               Densidad
#>  componente-0003  Completitud               Densidad
#>  componente-0004  Completitud               Densidad
#>  componente-0005  Completitud               Densidad
#>  componente-0006    Exactitud Correctitud sintáctica
#>  componente-0007    Exactitud Correctitud sintáctica
#>  componente-0008    Exactitud Correctitud sintáctica
#>  componente-0009    Exactitud Correctitud sintáctica
#>  componente-0010    Exactitud Correctitud sintáctica
#>  componente-0011 Consistencia  Integridad de dominio
#>                      metrica           objeto     valor orientacion agregacion
#>             EntidadDuplicada          (tabla) 0.1538462     defecto      ratio
#>                       NoNulo           cedula 1.0000000 conformidad      ratio
#>                       NoNulo fecha_nacimiento 1.0000000 conformidad      ratio
#>                       NoNulo          ingreso 1.0000000 conformidad      ratio
#>                       NoNulo             sexo 1.0000000 conformidad      ratio
#>                      Formato           correo 0.8461538 conformidad      ratio
#>                      Formato     departamento 0.9230769 conformidad      ratio
#>                      Formato       id_tramite 1.0000000 conformidad      ratio
#>                      Formato             pais 1.0000000 conformidad      ratio
#>                      Formato             sexo 0.8461538 conformidad      ratio
#>  ValoresPosiblesPorExtension             sexo 1.0000000 conformidad      ratio
#>  umbral universo
#>      NA    filas
#>      NA   celdas
#>      NA   celdas
#>      NA   celdas
#>      NA   celdas
#>      NA   celdas
#>      NA   celdas
#>      NA   celdas
#>      NA   celdas
#>      NA   celdas
#>      NA   celdas
#> 
#> ── Alcance del marco ──
#> 
#>  factores_marco factores_medidos sin_metrica_declarada no_aplican
#>              17                4                     8          4
#>  fuera_de_alcance
#>                 1
#> ── Cobertura conceptual ──
#> 
#>            estado factores
#>            medida        4
#>      no_declarada        8
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
