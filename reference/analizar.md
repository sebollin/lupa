# Ejecutar el análisis descriptivo completo

Es la puerta de entrada al recorrido de `lupa`. Construye el perfil, las
distribuciones, asociaciones, diagnóstico temporal, clasificación
confirmable de variables, propuesta de modelo, medición agregada,
tablero, cobertura conceptual y plan de limpieza. No modifica los datos.

## Usage

``` r
analizar(
  datos,
  nombre = .nombre_de_los_datos(substitute(datos)),
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
  proteger_datos_personales = FALSE,
  ...
)
```

## Arguments

- datos:

  Tabla que se desea analizar.

- nombre:

  Nombre de la entrega.

- fecha:

  Fecha y hora reproducible de la corrida. Se lee una vez, como en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  y [`medir()`](https://sebollin.github.io/lupa/reference/medir.md): en
  UTC, también un texto como `"2026-03-01"`. Si se omite, el perfil y la
  medición lo registran igual que esas dos funciones
  (`fecha_declarada = FALSE`), y
  [`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md)
  no les exige el orden temporal: la hora de la corrida no dice nada
  sobre las entregas.

- argumentos_perfil:

  Lista de argumentos adicionales para
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
  Los nombres se completan como los completa
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  —`aplic` es `aplicabilidad`— antes de leerlos, así que un nombre
  abreviado gobierna el perfil y la medición por igual.

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
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md). Si
  `argumentos_perfil` declara `aplicabilidad`, esa misma declaración
  viaja a la medición: el universo se declara una vez y gobierna todo el
  análisis, así que el perfil y el tablero cuentan el mismo dato de la
  misma manera. `distribuciones` recibe esa declaración y la de
  `sentinelas_numericos`: lo que queda fuera del universo no entra en
  sus frecuencias ni en sus cuantiles, y un centinela declarado sale de
  los cuantiles como sale de `minimo` y `maximo` del perfil.

- medir_propuesta:

  Si se mide automáticamente la propuesta con estado `"lista"` e
  `incluir = TRUE` cuando no se recibe un modelo o una propuesta
  confirmada. Con `FALSE`, `analizar()` describe y no mide.

- conservar_detalle_medicion:

  Si se retienen las medidas fila a fila. Es `FALSE` por omisión: el
  tablero y la medición agregada permanecen, y la medición se hace
  métrica por métrica, agregando cada una antes de pasar a la siguiente,
  así que el detalle entero no existe nunca a la vez. Con `TRUE` el
  detalle es parte del resultado y su memoria crece con las filas por
  las métricas: una tabla de 19 columnas producía 21 filas de medida por
  fila de datos. Si ninguna métrica del modelo produce medidas, el
  tablero sale vacío y lleva el motivo de cada una en
  `cobertura_metricas`, en lugar de abortar el análisis.

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
  personales de esa copia también se enmascaran, y en las demás columnas
  se tapa el valor protegido que identifica —el piso de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)—:
  la cédula citada en una observación, `"pago CI 3.738.538-0"`, o
  guardada como número en otra columna. En todo lugar donde un
  `data.frame` guarda un valor: las celdas de cualquier clase —una copia
  `POSIXct` de la fecha con hora protegida queda en `NA`, como la tapa
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)—,
  las columnas de lista elemento por elemento, los atributos de la tabla
  y de cada columna —también `levels` en lo que no es un factor y
  `units`, y el nombre de cada atributo, además de su valor—, los
  nombres de columna de una tabla anidada en una columna lista, y los
  nombres de fila, que vuelven a ser los números de fila si alguno lleva
  un valor protegido. Una columna `POSIXlt` se tapa como la `POSIXct`,
  con sus atributos —`comment`, `label`—, y cualquier lista con clase
  —un registro de `vctrs`, un `blob`— por sus valores; un BLOB (`raw`)
  se lee como el texto que guarda y, si lleva un valor protegido, la
  copia guarda los bytes del texto tapado; un entorno no se modifica —es
  del usuario—: si guarda un valor protegido, en su contenido o en sus
  atributos —valor y nombre, también `name`, que
  [`print()`](https://rdrr.io/r/base/print.html) muestra—, la copia
  lleva `"[valor protegido]"` en su lugar, también si tiene un atributo
  `name`; y también si no se puede leer entero —una promesa o una
  ligadura activa que aborta al leerse—, porque lo que no se lee no se
  sabe limpio; sólo el global, el de un paquete y los de la ruta de
  búsqueda se dejan como están. Una llamada, una fórmula o una función
  se leen como el texto que
  [`print()`](https://rdrr.io/r/base/print.html) escribe —con sus
  atributos y, si guarda su fuente (`srcref`), la fuente con sus
  comentarios—, y también por sus atributos y, la función, por su
  entorno; si algo de eso lleva un valor protegido, la copia lleva
  `"[valor protegido]"` en su lugar. Una columna protegida conserva sólo
  los atributos de su clase (`class`, `tzone`, `units`, `dim`), y
  `tzone` y `units` pasan por el piso: las etiquetas de valor de un
  `haven_labelled` se quitan. Para conservar sus valores debe usarse
  `proteger_datos_personales = FALSE` al crear el analisis y al
  guardarlo. La opcion del guardado no recupera valores que el analisis
  ya enmascaro.

- proteger_datos_personales:

  Si perfiles y resúmenes ocultan valores de columnas cuya clasificación
  activa protección automática, incluidos estadísticos de orden,
  cuantiles y rangos temporales. `FALSE` por omisión.

- ...:

  Argumentos con nombre enviados a
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
  Es una alternativa concisa a `argumentos_perfil`.

## Value

Objeto S3 `analisis` con todos los componentes y su cobertura.

Tres diferencias respecto de llamar a las funciones sueltas, y las tres
son deliberadas. La evaluación usa la medición **agregada**, no las
medidas por fila: `evaluar(medir(...), perfil)` aplica la regla a cada
medida y puede dar otra cifra que `analizar(perfil_evaluacion = perfil)`
sobre los mismos datos —una proporción de medidas contra una proporción
de métricas—, y sobre medidas de `0` o `1` los tres
[`perfiles_madurez()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md)
no se distinguen. Para reproducirla con las funciones sueltas,
[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)
antes de
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md). La
entrada puede ser una matriz de dos dimensiones: se la pasa a
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
sin convertir, para que sea él quien la convierta y lo declare en
`meta$entrada_convertida`. Y cuando una columna temporal pertenece a una
columna protegida, `temporal$resumen` gana la columna
`proteccion_temporal` con el texto `[rangos y huecos protegidos]`: la
protección se aplica y se declara en la misma tabla donde faltan los
valores. La llamada directa a
[`analizar_tiempo()`](https://sebollin.github.io/lupa/reference/analizar_tiempo.md)
respeta la misma declaración cuando recibe ese `perfil`.

## Details

Por omisión mide las sugerencias de la propuesta cuyo estado es
`"lista"` y cuya marca `incluir` es `TRUE`; los patrones y dominios
observados que
[`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md)
deja inactivos no llegan al tablero sin confirmación. Declara que esa
selección fue realizada por `lupa` sin confirmación. Agrega
inmediatamente el detalle y conserva
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
# Cuatro columnas de la tabla de ejemplo, para que corra en pocos segundos.
columnas <- c("fecha_nacimiento", "sexo", "ingreso", "departamento")
resultado <- analizar(
  datos_administrativos[, columnas], analizar_dependencias = FALSE,
  fecha = as.POSIXct("2026-01-15", tz = "UTC")
)
resultado
#> 
#> ── Analisis de datos: datos_administrativos[, columnas] ────────────────────────
#> Filas: 13
#> Columnas: 4
#> Hallazgos del perfil: 10
#> Advertencias de alcance: 2
#> Asociaciones informadas: 1
#> Series temporales: 1
#> Modelo medido: si
#> Detalle fila a fila: no conservado
#> 
#> ── Tablero de calidad ──
#> 
#> ── Tablero de calidad ──────────────────────────────────────────────────────────
#>       componente   dimension         factor          metrica
#>  componente-0001    Unicidad No-duplicación EntidadDuplicada
#>  componente-0002 Completitud       Densidad           NoNulo
#>  componente-0003 Completitud       Densidad           NoNulo
#>  componente-0004 Completitud       Densidad           NoNulo
#>      metrica_instanciada                           entidad           objeto
#>         EntidadDuplicada datos_administrativos[, columnas]          (tabla)
#>  NoNulo:fecha_nacimiento datos_administrativos[, columnas] fecha_nacimiento
#>           NoNulo:ingreso datos_administrativos[, columnas]          ingreso
#>              NoNulo:sexo datos_administrativos[, columnas]             sexo
#>      valor orientacion agregacion umbral universo
#>  0.1538462     defecto      ratio     NA    filas
#>  0.9230769 conformidad      ratio     NA   celdas
#>  0.9230769 conformidad      ratio     NA   celdas
#>  0.8461538 conformidad      ratio     NA   celdas
#> 
#> ── Alcance del marco ──
#> 
#>  factores_marco factores_medidos sin_metrica_declarada no_aplican
#>              17                2                    10          4
#>  fuera_de_alcance medidos_fuera_del_marco
#>                 1                       0
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
