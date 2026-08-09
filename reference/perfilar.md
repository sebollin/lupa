# Perfilar un conjunto de datos

Examina un `data.frame`, `tibble` o `data.table` y devuelve estadísticas
generales, métricas por columna, patrones, formatos de fecha y hallazgos
accionables. Todas las proporciones se expresan en `[0, 1]`.

## Uso

``` r
perfilar(
  datos,
  nombre = deparse(substitute(datos)),
  fecha = Sys.time(),
  muestra = 1e+05,
  max_patrones = 20,
  distinguir_mayusculas = TRUE,
  expandir = FALSE,
  umbral_alta_cardinalidad = 0.5,
  umbral_faltantes_sospechoso = 0.1,
  umbral_faltantes_error = 0.4,
  umbral_patron_raro = 0.05,
  umbral_patron_dominante = 0.5,
  columnas_sin_ceros = character(),
  columnas_no_negativas = character(),
  sentinelas_numericos = c(-9, -99, -999, -9999, 999),
  analizar_dependencias = TRUE,
  umbral_dependencia = 0.995,
  umbral_casi_clave_dependencia = 0.8,
  max_columnas_dependencias = 100L,
  datos_personales_permitidos = TRUE,
  proteger_datos_personales = TRUE,
  validadores_personales = NULL,
  umbral_documento_verificado = 0.9,
  muestra_validadores = 1000L,
  duplicados_aproximados = FALSE,
  max_filas_hallazgo = 1000L
)
```

## Argumentos

  - datos:
    
    Objeto que hereda de `data.frame`.

  - nombre:
    
    Nombre descriptivo del objeto.

  - fecha:
    
    Fecha y hora de la corrida. Se puede fijar para construir series
    reproducibles; se normaliza a UTC.

  - muestra:
    
    Máximo de filas usadas para patrones e inferencia de tipos. Use
    `Inf` para analizar todas las filas.

  - max\_patrones:
    
    Máximo de patrones mostrados por columna.

  - distinguir\_mayusculas:
    
    Si se distinguen mayúsculas y minúsculas.

  - expandir:
    
    Si se emite un token por carácter en los patrones.

  - umbral\_alta\_cardinalidad:
    
    Umbral para columnas categóricas.

  - umbral\_faltantes\_sospechoso:
    
    Umbral inferior de faltantes. El hallazgo se activa al superarlo en
    sentido estricto.

  - umbral\_faltantes\_error:
    
    Umbral por encima del cual los faltantes son un error; la igualdad
    conserva la severidad sospechosa.

  - umbral\_patron\_raro:
    
    Máxima frecuencia de un patrón raro.

  - umbral\_patron\_dominante:
    
    Frecuencia mínima del patrón dominante.

  - columnas\_sin\_ceros:
    
    Nombres de columnas donde cero no es admisible.

  - columnas\_no\_negativas:
    
    Nombres de columnas que deben ser no negativas.

  - sentinelas\_numericos:
    
    Vector completo de valores numéricos que se interpretan como
    ausencia. `numeric()` los desactiva; las cadenas de ausencia se
    siguen evaluando por separado.

  - analizar\_dependencias:
    
    Si se buscan dependencias funcionales entre pares de columnas. Se
    aplica una sola muestra común a toda la tabla.

  - umbral\_dependencia:
    
    Cumplimiento mínimo para informar una dependencia.

  - umbral\_casi\_clave\_dependencia:
    
    Tasa de valores distintos a partir de la cual un determinante se
    descarta como casi-clave antes de agrupar.

  - max\_columnas\_dependencias:
    
    Máximo de columnas que intervienen en la búsqueda, cuyo costo crece
    cuadráticamente.

  - datos\_personales\_permitidos:
    
    Si la entrega admite datos personales. El valor predeterminado no
    juzga su presencia: la clasificación se informa con severidad
    `"ok"`. Use `FALSE` sólo cuando el contrato de la entrega declare
    que no deben existir.

  - proteger\_datos\_personales:
    
    Si se reemplazan modas, ejemplos, evidencia y estadísticos de orden
    concretos cuando `poder_discriminante` es medio, alto o verificado.
    Las clasificaciones débiles se conservan como aviso pero no suprimen
    estadísticos. Para conservar todo en el objeto debe desactivarse
    explícitamente; `reportar()` aplica además su propia protección
    predeterminada.

  - validadores\_personales:
    
    Pack o lista nombrada de funciones que reciben un vector de texto y
    devuelven un lógico de igual longitud. `NULL` usa
    `validadores_uruguay()` por compatibilidad; `FALSE` o `numeric()`
    desactiva la verificación de documentos. El nombre del mejor
    validador queda en el fundamento de la clasificación.

  - umbral\_documento\_verificado:
    
    Proporción mínima de valores que debe aceptar un validador para
    clasificar una forma de documento como `verificado`. Por defecto es
    `0.9`.

  - muestra\_validadores:
    
    Máximo de valores usados en el filtro preliminar de cada validador.
    Si la proporción preliminar ya queda bajo el umbral no se valida la
    columna completa; use `Inf` para revisar todos desde el inicio.

  - duplicados\_aproximados:
    
    `FALSE` por omisión. Use `TRUE` o una lista de argumentos para
    ejecutar `detectar_duplicados_aproximados()` y añadir sus pares y
    hallazgos al perfil. Es un análisis acotado y opcional porque no
    afirma identidad ni debe encarecer todas las corridas.

  - max\_filas\_hallazgo:
    
    Tope de índices de fila que conserva cada trazabilidad disponible.
    Por defecto es `1000`; cuando se supera, el estado queda como
    `truncada` y el total se conserva. Use `Inf` sólo si necesita
    desactivar explícitamente el tope.

## Valor

Objeto S3 de clase `perfil`. Cada fila de hallazgos incluye
n\_evaluados, n\_afectados y unidad\_conteo: son conteos de las unidades
declaradas (por ejemplo fila, columna, formato o par). Cuando el camino
no puede conocer un conteo, informa NA, nunca cero. La columna de lista
`trazabilidad` distingue `disponible`, `truncada`, `no_aplica` y
`no_disponible`; cuando corresponde conserva índices de fila acotados
por `max_filas_hallazgo`, el total conocido y el alcance (completo o
parcial). Los índices no contienen valores. Usarlos para extraer filas
de los datos originales puede volver a exponer datos personales; el
paquete no realiza esa extracción y la protección de salidas no
sustituye el control de acceso a los datos de entrada.

## Detalles

Los umbrales de faltantes se aplican a la suma de ausentes reales y
faltantes disfrazados y son estrictos: la proporción debe superar el
umbral para generar el nivel correspondiente. La lista de cadenas está
congelada con referencia a
[naniar](https://github.com/njtierney/naniar)::common\_na\_strings 1.1.0
y suma extensiones habituales en datos administrativos uruguayos. Las
entradas que [naniar](https://github.com/njtierney/naniar) expresa como
patrones escapados se adaptan a los signos literales de interrogación,
asterisco y punto porque aquí se comparan por igualdad. La lista no
depende de la versión instalada. Los sentinelas numéricos
predeterminados son `-9`, `-99`, `-999`, `-9999` y `999`. La lista es
deliberadamente más corta que
[naniar](https://github.com/njtierney/naniar)::common\_na\_numbers
1.1.0: `66`, `77`, `88` y `9999` también pueden ser edades, códigos o
años legítimos. `sentinelas_numericos` representa la política completa,
no una lista que se agrega silenciosamente: use `numeric()` para
desactivar todos los sentinelas numéricos, o `sentinelas_naniar` para
solicitar explícitamente la lista de naniar.

`muestra` limita sólo el descubrimiento de patrones, la inferencia de
tipos y la detección de formatos de fecha. Las demás métricas y
hallazgos se calculan sobre todas las filas. Por eso
`meta$filas_analizadas` describe el máximo usado por los análisis
muestreados, no el alcance del perfil completo.

Una columna cuyo año se expresa con dos dígitos se informa con su
`tipo_inferido` —`"fecha"` o `"fecha-hora"`— pero deja `minimo_fecha`,
`maximo_fecha`, `media_fecha` y `mediana_fecha` en `NA`. No es una
omisión: `23` puede ser 1923 o 2023, y elegir el siglo para calcular un
rango sería inventarlo. El hallazgo `anio_de_dos_digitos` señala esas
columnas, y el rango aparece una vez que el usuario resuelve la
ambigüedad.

Para números ordinarios, los estadísticos cuantitativos se calculan sólo
con valores finitos; `n_nan`, `n_infinito_positivo` y
`n_infinito_negativo` declaran lo excluido. En columnas `integer64` que
exceden el entero máximo representable exactamente por `double`,
`minimo` y `maximo` quedan en `NA` y los extremos exactos se conservan
en `minimo_exacto` y `maximo_exacto`. Una columna de listas intenta
contar sus valores distintos; si la clase no admite comparación, informa
`NA` en lugar de afirmar cero. Las columnas matriciales se conservan
como una unidad por fila: `n` informa las filas de la tabla, pero los
estadísticos por valor quedan en `NA` y un hallazgo explica que deben
separarse en columnas con semántica explícita. Los valores de texto que
no forman UTF-8 válido tampoco se convierten: se cuentan, se excluyen de
los análisis textuales y generan un hallazgo con sus posiciones.

Los resúmenes de fecha-hora se expresan siempre en UTC y llevan el
sufijo `UTC` en el texto para hacer visible la zona aplicada. El
instante se conserva aunque la columna de entrada use otra zona horaria.

La normalización Unicode se compara sin modificar el texto y requiere el
paquete opcional `stringi` sólo cuando existen caracteres no ASCII.

La clasificación de posibles datos personales es más amplia que la
protección. Cada clasificación declara `poder_discriminante` y
`proteger`:

  - `debil`: una forma genérica, como siete a doce dígitos, coincide
    también con importes, facturas y códigos; se informa pero no se
    ocultan valores;

  - `medio`: el nombre de la columna expresa una categoría personal (por
    ejemplo `telefono` o `fecha_nacimiento`); se protege aunque sus
    valores no se puedan validar. El nombre tiene prioridad sobre una
    forma numérica genérica y también determina la etiqueta de tipo;

  - `alto`: una forma muy específica, como un correo, o nombre y forma
    se apoyan mutuamente; se protege;

  - `verificado`: al menos tres valores distintos y al menos el 90%
    cumple uno de los validadores personales configurados; se protege
    incluso sin un nombre orientador. El pack uruguayo es el
    predeterminado, pero puede reemplazarse por un `pack_validadores()`
    de otro país o desactivarse con `FALSE`. La tolerancia del 10%
    permite tipeos aislados sin convertir una columna real en una salida
    pública; el umbral es configurable.

La forma genérica de siete a doce dígitos tiene poder discriminante
débil: también describe importes, teléfonos, facturas e
identificadores. Las formas con separadores sólo se aceptan cuando
tienen una estructura de documento reconocible (por ejemplo, una cédula
con grupos y guion o un RUT con grupos de tres y cuatro dígitos); una
fecha ISO, una fecha con puntos o guiones y separadores arbitrarios no
se consideran documentos. Un validador de dígito que supera el umbral
aporta evidencia verificable y eleva la clasificación; una forma sola
nunca se trata como prueba de identidad.

Este criterio mide capacidad de discriminación, no juzga si la presencia
del dato es correcta. La protección sustituye modas, ejemplos, evidencia
y extremos o medianas que corresponden a observaciones reales. Las
medias y desvíos se conservan como síntesis no ligadas a una fila;
`detalle_proteccion_personal` hace visible la supresión. En fechas de
nacimiento, un hallazgo separado conserva el diagnóstico de valores
anteriores a 1900 o posteriores a la corrida sin publicar las fechas.
Los números escritos como texto reconocen tanto coma como punto decimal
y sus separadores de miles simétricos. Los prefijos de tres letras
separados del número, con forma de código ISO 4217, y los símbolos
monetarios se conservan como evidencia; una columna sin datos
suficientes para desambiguar un separador de tres dígitos no se
convierte automáticamente.

## Ver también

`descubrir_patrones()`, `detectar_dependencias()`, `proponer_modelo()`,
`planificar_limpieza()`

## Ejemplos

``` r
perfil <- perfilar(datos_administrativos)
perfil
#> 
#> ── Perfil de datos: datos_administrativos ──────────────────────────────────────
#> ✖ 5 hallazgos con severidad error
#> ! 10 hallazgos sospechosos
#> ✔ 4 hallazgos informativos ok
#> 
#> ── Resumen general ──
#> 
#> Filas: 13
#> Columnas: 10
#> Celdas: 130
#> Filas completas: 13
#> Filas duplicadas: 1
#> Memoria: 7.1 Kb
#> 
#> ── Resumen por columna ──
#> 
#>           columna tipo_declarado tipo_inferido prop_faltantes_totales
#>        id_persona          doble         doble             0.00000000
#>            cedula          texto         texto             0.07692308
#>  fecha_nacimiento          texto         fecha             0.07692308
#>              sexo          texto         texto             0.15384615
#>           ingreso          doble         doble             0.07692308
#>      departamento          texto         texto             0.00000000
#>              pais          texto         texto             0.00000000
#>            correo          texto         texto             0.00000000
#>          id_copia          doble         doble             0.00000000
#>        id_tramite          texto identificador             0.00000000
#>  n_distintos n_outliers
#>           11          0
#>           11         NA
#>           12          0
#>            4         NA
#>           12          4
#>           11         NA
#>            1         NA
#>           12         NA
#>           11          0
#>           12         NA
summary(perfil)
#>             columna tipo_declarado tipo_inferido proporcion_tipo_inferido  n
#> 1        id_persona          doble         doble                1.0000000 13
#> 2            cedula          texto         texto                1.0000000 13
#> 3  fecha_nacimiento          texto         fecha                0.8461538 13
#> 4              sexo          texto         texto                1.0000000 13
#> 5           ingreso          doble         doble                1.0000000 13
#> 6      departamento          texto         texto                1.0000000 13
#> 7              pais          texto         texto                1.0000000 13
#> 8            correo          texto         texto                1.0000000 13
#> 9          id_copia          doble         doble                1.0000000 13
#> 10       id_tramite          texto identificador                1.0000000 13
#>    n_faltantes prop_faltantes n_faltantes_disfrazados
#> 1            0              0                       0
#> 2            0              0                       1
#> 3            0              0                       1
#> 4            0              0                       2
#> 5            0              0                       1
#> 6            0              0                       0
#> 7            0              0                       0
#> 8            0              0                       0
#> 9            0              0                       0
#> 10           0              0                       0
#>    n_faltantes_disfrazados_textuales n_faltantes_disfrazados_numericos
#> 1                                  0                                 0
#> 2                                  1                                 0
#> 3                                  1                                 0
#> 4                                  2                                 0
#> 5                                  0                                 1
#> 6                                  0                                 0
#> 7                                  0                                 0
#> 8                                  0                                 0
#> 9                                  0                                 0
#> 10                                 0                                 0
#>    prop_faltantes_disfrazados n_faltantes_totales prop_faltantes_totales
#> 1                  0.00000000                   0             0.00000000
#> 2                  0.07692308                   1             0.07692308
#> 3                  0.07692308                   1             0.07692308
#> 4                  0.15384615                   2             0.15384615
#> 5                  0.07692308                   1             0.07692308
#> 6                  0.00000000                   0             0.00000000
#> 7                  0.00000000                   0             0.00000000
#> 8                  0.00000000                   0             0.00000000
#> 9                  0.00000000                   0             0.00000000
#> 10                 0.00000000                   0             0.00000000
#>    n_distintos tasa_distintos              moda frecuencia_moda longitud_minima
#> 1           11     0.84615385                 1               2              NA
#> 2           11     0.84615385 [valor protegido]               2               3
#> 3           12     0.92307692 [valor protegido]               2               4
#> 4            4     0.30769231                 F               6               0
#> 5           12     0.92307692             25000               2              NA
#> 6           11     0.84615385        Montevideo               2               5
#> 7            1     0.07692308                UY              13               2
#> 8           12     0.92307692 [valor protegido]               2              10
#> 9           11     0.84615385                 1               2              NA
#> 10          12     0.92307692             TR001               2               5
#>    longitud_maxima longitud_media minimo  maximo        media mediana
#> 1               NA             NA      1      11 5.923077e+00       6
#> 2               11       9.692308     NA      NA           NA      NA
#> 3               10       9.384615     NA      NA           NA      NA
#> 4                3       1.076923     NA      NA           NA      NA
#> 5               NA             NA    -99 9999999 7.900192e+05   29900
#> 6               10       7.461538     NA      NA           NA      NA
#> 7                2       2.000000     NA      NA           NA      NA
#> 8               26      23.923077     NA      NA           NA      NA
#> 9               NA             NA      1      11 5.923077e+00       6
#> 10               5       5.000000     NA      NA           NA      NA
#>          desvio minimo_exacto maximo_exacto      minimo_fecha      maximo_fecha
#> 1  3.546396e+00          <NA>          <NA>              <NA>              <NA>
#> 2            NA          <NA>          <NA>              <NA>              <NA>
#> 3  1.191710e+08          <NA>          <NA> [valor protegido] [valor protegido]
#> 4            NA          <NA>          <NA>              <NA>              <NA>
#> 5  2.767287e+06          <NA>          <NA>              <NA>              <NA>
#> 6            NA          <NA>          <NA>              <NA>              <NA>
#> 7            NA          <NA>          <NA>              <NA>              <NA>
#> 8            NA          <NA>          <NA>              <NA>              <NA>
#> 9  3.546396e+00          <NA>          <NA>              <NA>              <NA>
#> 10           NA          <NA>          <NA>              <NA>              <NA>
#>    media_fecha     mediana_fecha n_ceros n_negativos n_outliers n_nan
#> 1         <NA>              <NA>       0           0          0     0
#> 2         <NA>              <NA>      NA          NA         NA     0
#> 3   1984-12-02 [valor protegido]       0           0          0     0
#> 4         <NA>              <NA>      NA          NA         NA     0
#> 5         <NA>              <NA>       1           2          4     0
#> 6         <NA>              <NA>      NA          NA         NA     0
#> 7         <NA>              <NA>      NA          NA         NA     0
#> 8         <NA>              <NA>      NA          NA         NA     0
#> 9         <NA>              <NA>       0           0          0     0
#> 10        <NA>              <NA>      NA          NA         NA     0
#>    n_infinito_positivo n_infinito_negativo estado_resumen_cuantitativo
#> 1                    0                   0                  calculados
#> 2                    0                   0                   no_aplica
#> 3                    0                   0                  calculados
#> 4                    0                   0                   no_aplica
#> 5                    0                   0                  calculados
#> 6                    0                   0                   no_aplica
#> 7                    0                   0                   no_aplica
#> 8                    0                   0                   no_aplica
#> 9                    0                   0                  calculados
#> 10                   0                   0                   no_aplica
#>           detalle_proteccion_personal n_blancos n_espacios_borde
#> 1                                <NA>         0                0
#> 2                                <NA>         0                0
#> 3  [estadisticos de orden protegidos]         0                0
#> 4                                <NA>         1                0
#> 5                                <NA>         0                0
#> 6                                <NA>         0                0
#> 7                                <NA>         0                0
#> 8                                <NA>         0                0
#> 9                                <NA>         0                0
#> 10                               <NA>         0                0
#>    n_variantes_mayusculas n_variantes_unicode n_codificacion_rota
#> 1                       0                  NA                   0
#> 2                       0                   0                   0
#> 3                       0                   0                   0
#> 4                       0                   0                   0
#> 5                       0                  NA                   0
#> 6                       0                   0                   0
#> 7                       0                   0                   0
#> 8                       0                   0                   0
#> 9                       0                  NA                   0
#> 10                      0                   0                   0
#>    n_codificacion_reparable n_codificacion_reparable_parcialmente
#> 1                         0                                     0
#> 2                         0                                     0
#> 3                         0                                     0
#> 4                         0                                     0
#> 5                         0                                     0
#> 6                         0                                     0
#> 7                         0                                     0
#> 8                         0                                     0
#> 9                         0                                     0
#> 10                        0                                     0
#>    n_codificacion_irreparable n_codificacion_no_se_pudo
#> 1                           0                         0
#> 2                           0                         0
#> 3                           0                         0
#> 4                           0                         0
#> 5                           0                         0
#> 6                           0                         0
#> 7                           0                         0
#> 8                           0                         0
#> 9                           0                         0
#> 10                          0                         0
#>    estado_codificacion_reparacion n_codificacion_invalida n_numeros_texto
#> 1                  no_parece_roto                       0               0
#> 2                  no_parece_roto                       0               0
#> 3                  no_parece_roto                       0               0
#> 4                  no_parece_roto                       0               0
#> 5                  no_parece_roto                       0               0
#> 6                  no_parece_roto                       0               0
#> 7                  no_parece_roto                       0               0
#> 8                  no_parece_roto                       0               0
#> 9                  no_parece_roto                       0               0
#> 10                 no_parece_roto                       0               0
#>    proporcion_numeros_texto numero_texto_ambiguo numero_texto_seguro
#> 1                        NA                FALSE               FALSE
#> 2                        NA                FALSE               FALSE
#> 3                        NA                FALSE               FALSE
#> 4                        NA                FALSE               FALSE
#> 5                        NA                FALSE               FALSE
#> 6                        NA                FALSE               FALSE
#> 7                        NA                FALSE               FALSE
#> 8                        NA                FALSE               FALSE
#> 9                        NA                FALSE               FALSE
#> 10                       NA                FALSE               FALSE
#>    numero_texto_unidad numero_texto_moneda numero_texto_convencion
#> 1                                                                 
#> 2                                                                 
#> 3                                                                 
#> 4                                                                 
#> 5                                                                 
#> 6                                                                 
#> 7                                                                 
#> 8                                                                 
#> 9                                                                 
#> 10                                                                
#>    dato_personal_posible  tipo_dato_personal proporcion_dato_personal
#> 1                  FALSE                <NA>                       NA
#> 2                   TRUE documento_identidad                0.8461538
#> 3                   TRUE    fecha_nacimiento                1.0000000
#> 4                  FALSE                <NA>                       NA
#> 5                  FALSE                <NA>                       NA
#> 6                  FALSE                <NA>                       NA
#> 7                  FALSE                <NA>                       NA
#> 8                   TRUE              correo                0.9230769
#> 9                  FALSE                <NA>                       NA
#> 10                 FALSE                <NA>                       NA
#>    poder_discriminante_dato_personal dato_personal_protegido
#> 1                               <NA>                   FALSE
#> 2                               alto                    TRUE
#> 3                              medio                    TRUE
#> 4                               <NA>                   FALSE
#> 5                               <NA>                   FALSE
#> 6                               <NA>                   FALSE
#> 7                               <NA>                   FALSE
#> 8                               alto                    TRUE
#> 9                               <NA>                   FALSE
#> 10                              <NA>                   FALSE
```
