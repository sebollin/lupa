# Estructura no declarada

Una entrega puede llegar sin diccionario, claves ni relaciones
declaradas. Esta guía muestra cómo examinar la estructura que se cumple
en los datos recibidos y qué parte de esa estructura todavía requiere
confirmación.

## Dos tablas que se pueden seguir fila por fila

Los datos incluidos en `lupa` tienen trece filas cada uno. Sirven para
examinar tipos, formatos, distribuciones, asociaciones y tiempo, pero no
contienen una clave exacta: por ejemplo, `id_evento` tiene doce valores
distintos en trece filas de `datos_operativos`. Por eso
[`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md)
y
[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md)
no se muestran sobre esos conjuntos: devolverían tablas sin filas.

Para ver una clave, una relación y una dependencia se construyen dos
tablas chicas. Cada conclusión se puede contrastar con las filas
impresas.

``` r

library(lupa)

personas <- data.frame(
  id_persona = 101:108,
  nombre = rep(c("Ana", "Bruno", "Carla", "Diego"), each = 2),
  ciudad = rep(c("Montevideo", "Salto", "Belén", "Melo"), each = 2),
  departamento = rep(
    c("Montevideo", "Salto", "Salto", "Cerro Largo"), each = 2
  ),
  stringsAsFactors = FALSE
)

tramites <- data.frame(
  id_tramite = 1001:1010,
  id_persona = c(101, 101, 102, 103, 104, 104, 105, 106, 107, 108),
  tipo = rep(c("alta", "renovación"), 5),
  stringsAsFactors = FALSE
)

personas
#>   id_persona nombre     ciudad departamento
#> 1        101    Ana Montevideo   Montevideo
#> 2        102    Ana Montevideo   Montevideo
#> 3        103  Bruno      Salto        Salto
#> 4        104  Bruno      Salto        Salto
#> 5        105  Carla      Belén        Salto
#> 6        106  Carla      Belén        Salto
#> 7        107  Diego       Melo  Cerro Largo
#> 8        108  Diego       Melo  Cerro Largo
tramites
#>    id_tramite id_persona       tipo
#> 1        1001        101       alta
#> 2        1002        101 renovación
#> 3        1003        102       alta
#> 4        1004        103 renovación
#> 5        1005        104       alta
#> 6        1006        104 renovación
#> 7        1007        105       alta
#> 8        1008        106 renovación
#> 9        1009        107       alta
#> 10       1010        108 renovación
```

`id_persona` no falta y no se repite en `personas`. Se limita la
búsqueda a columnas simples para que la salida responda sólo esa
pregunta.

``` r

claves <- detectar_claves(personas, max_combinacion = 1)
claves[, c(
  "columnas", "n_columnas", "n_filas", "casi_clave", "unicidad_exacta"
)]
#>     columnas n_columnas n_filas casi_clave unicidad_exacta
#> 1 id_persona          1       8      FALSE            TRUE
```

La misma columna se repite en `tramites`: una persona puede tener más de
un trámite.
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
necesita las dos tablas. Al comparar las dos columnas de identificación
encuentra una cardinalidad 1:m; ambas coberturas son 1 porque todos los
identificadores aparecen en el otro lado.

``` r

detectar_relaciones(
  personas["id_persona"], tramites["id_persona"], muestra = Inf
)
#>   columna_tabla1 columna_tabla2 cardinalidad n_valores_comunes cobertura_tabla1_en_tabla2
#> 1     id_persona     id_persona          1:m                 8                          1
#>   cobertura_tabla2_en_tabla1 motivo_poda
#> 1                          1        <NA>
```

En `personas`, cada ciudad observada pertenece a un solo departamento.
Salto y Belén muestran que la relación inversa no se cumple. El mínimo
de observaciones se baja a cuatro porque la tabla construida tiene ocho
filas; `muestra = Inf` deja explícito que se usan todas.

``` r

dependencias <- detectar_dependencias(
  personas[c("ciudad", "departamento")],
  muestra = Inf,
  min_observaciones = 4
)
dependencias
#>   determinante  dependiente cumplimiento n_evaluados n_grupos n_violaciones exacta
#> 1       ciudad departamento            1           8        4             0   TRUE
#>   evidencia
#> 1
```

Nada de lo anterior modifica ni declara el modelo. La clave es
*candidata*, la cardinalidad describe estas dos tablas y la dependencia
se cumple en estas ocho filas. Una entrega futura puede contradecirlas;
la documentación del sistema es la que puede confirmarlas como reglas.

## Patrones, tipos y fechas en los datos incluidos

Las funciones de columna reciben vectores. En particular,
[`descubrir_patrones()`](https://sebollin.github.io/lupa/reference/descubrir_patrones.md)
no recibe un data frame. Sobre `codigo_usuario`, la salida cuenta las
formas observadas y conserva ejemplos. El resumen conserva como máximo
seis patrones raros para presentar; el atributo
`patrones_raros_trazabilidad` conserva sus nombres hasta 5.000, sin
guardar la tabla de frecuencias.

``` r

descubrir_patrones(datos_operativos$codigo_usuario)
#>   patron  n proporcion                    ejemplos
#> 1  A+-9+ 10 0.76923077 USR-001 | USR-002 | USR-004
#> 2    A/A  1 0.07692308                         S/D
#> 3   A+-9  1 0.07692308                       MAL-5
#> 4   A+9+  1 0.07692308                     USR0003
```

El almacenamiento como texto no impide proponer un tipo implícito. La
fecha se examina como vector y los formatos se cuentan por separado.

``` r

inferir_tipo(datos_operativos$fecha_evento)
#> fecha (84.6%; 11 de 13 valores compatibles)
detectar_formatos_fecha(datos_operativos$fecha_evento)
#>    formato n proporcion     estado n_inequivocos n_ambiguos anio_dos_digitos granularidad
#> 1 %d/%m/%Y 4 0.30769231 confirmado             4          0            FALSE          dia
#> 2 %Y-%m-%d 4 0.30769231 confirmado             4          0            FALSE          dia
#> 3 %d-%m-%Y 1 0.07692308 confirmado             1          0            FALSE          dia
#> 4 %Y/%m/%d 1 0.07692308 confirmado             1          0            FALSE          dia
#> 5   %Y%m%d 1 0.07692308 confirmado             1          0            FALSE          dia
```

Estas salidas describen compatibilidad y frecuencia. No convierten la
columna ni afirman cuál formato debe aceptar el sistema.

Una columna compuesta sólo por fechas como `01/02/2020` puede ser
compatible al 100 % y seguir siendo ambigua: sin una convención externa
no se sabe si el primer número es día o mes. El perfil conserva esa
diferencia donde se consume la inferencia.

``` r

set.seed(20260831)
perfil_fecha_ambigua <- perfilar(
  data.frame(
    fecha_ambigua = c("01/02/2020", "02/03/2020", "03/04/2020"),
    stringsAsFactors = FALSE
  ),
  muestra = Inf,
  analizar_dependencias = FALSE
)
perfil_fecha_ambigua$columnas[, c(
  "columna", "tipo_inferido", "proporcion_tipo_inferido",
  "estado_tipo_inferido"
)]
#>         columna tipo_inferido proporcion_tipo_inferido estado_tipo_inferido
#> 1 fecha_ambigua         fecha                        1            candidato
```

Así, `tipo_inferido = "fecha"` y `proporcion_tipo_inferido = 1` no se
leen como confirmación: `estado_tipo_inferido = "candidato"` dice que la
lectura todavía no está establecida.

## Escalas, distribuciones y asociaciones

[`clasificar_variables()`](https://sebollin.github.io/lupa/reference/clasificar_variables.md)
propone una escala y un rol por columna. `confianza` expone la fuerza de
la propuesta y `confirmada` distingue una declaración de una inferencia.
Sin metadatos declarados, las propuestas siguientes quedan sin
confirmar.

``` r

clasificacion <- clasificar_variables(datos_operativos)
clasificacion[, c(
  "columna", "tipo_almacenamiento", "tipo_implicito", "escala_propuesta",
  "rol", "confianza", "confirmada"
)]
#>           columna tipo_almacenamiento tipo_implicito escala_propuesta           rol
#> 1     id_registro               doble          doble         discreta        medida
#> 2  codigo_usuario               texto          texto          nominal     categoria
#> 3    fecha_evento               texto          fecha         temporal         fecha
#> 4           canal               texto          texto          nominal     categoria
#> 5           monto               doble          doble         discreta        medida
#> 6            zona               texto          texto          nominal     categoria
#> 7         sistema               texto          texto          nominal     categoria
#> 8        contacto               texto          texto          nominal     categoria
#> 9        id_copia               doble          doble         discreta        medida
#> 10      id_evento               texto  identificador          nominal identificador
#>    confianza confirmada
#> 1       0.65      FALSE
#> 2       0.55      FALSE
#> 3       0.85      FALSE
#> 4       0.55      FALSE
#> 5       0.65      FALSE
#> 6       0.55      FALSE
#> 7       0.55      FALSE
#> 8       0.55      FALSE
#> 9       0.65      FALSE
#> 10      0.90      FALSE
```

Una distribución agrega evidencia sobre valores concretos y cuantiles.
Aquí se muestran las frecuencias de `canal` y los cuantiles de `monto`;
`alcance` del objeto conserva el total analizado, el muestreo y el
truncamiento por columna.

``` r

distribuciones <- distribucion_valores(datos_operativos, max_valores = 5)
distribuciones$frecuencias[
  distribuciones$frecuencias$columna == "canal", , drop = FALSE
]
#>    columna rango      valor frecuencia proporcion
#> 16   canal     1        web          5 0.38461538
#> 17   canal     2   telefono          2 0.15384615
#> 18   canal     3        Web          1 0.07692308
#> 19   canal     4 presencial          1 0.07692308
#> 20   canal     5        S/D          1 0.07692308
distribuciones$cuantiles[
  distribuciones$cuantiles$columna == "monto", , drop = FALSE
]
#>    columna probabilidad  valor n_analizados muestreado    estado
#> 6    monto         0.00    -99           13      FALSE calculado
#> 7    monto         0.25   1250           13      FALSE calculado
#> 8    monto         0.50   1490           13      FALSE calculado
#> 9    monto         0.75   1700           13      FALSE calculado
#> 10   monto         1.00 999999           13      FALSE calculado
```

Las asociaciones son medidas entre pares, no dependencias ni
instrucciones de modelado. La columna `supuesto` dice cómo se trataron
las escalas para calcular cada medida.

``` r

asociaciones <- detectar_asociaciones(datos_operativos)
utils::head(asociaciones, 5)
#>     columna_1 columna_2     tipo_1     tipo_2           metodo
#> 1 id_registro  id_copia   numerica   numerica pearson_absoluto
#> 2       canal     monto categorica   numerica             eta2
#> 3       canal      zona categorica categorica         cramer_v
#> 4 id_registro     monto   numerica   numerica pearson_absoluto
#> 5       monto  id_copia   numerica   numerica pearson_absoluto
#>                                                                              supuesto
#> 1 Las columnas numericas se tratan como cuantitativas; la escala no queda confirmada.
#> 2            La columna numerica se trata como cuantitativa y la otra como categoria.
#> 3                                   Las columnas se tratan como categorias sin orden.
#> 4 Las columnas numericas se tratan como cuantitativas; la escala no queda confirmada.
#> 5 Las columnas numericas se tratan como cuantitativas; la escala no queda confirmada.
#>   asociacion n_pares
#> 1  1.0000000      13
#> 2  0.9999977      13
#> 3  0.8944272      13
#> 4  0.4301569      13
#> 5  0.4301569      13
```

## Cobertura temporal

[`analizar_tiempo()`](https://sebollin.github.io/lupa/reference/analizar_tiempo.md)
reconoce `fecha_evento` mediante su contenido. Resume el período
observado y propone una frecuencia con `confirmada = FALSE`; la
propuesta sigue siendo evidencia para revisar.

``` r

tiempo <- analizar_tiempo(datos_operativos)
tiempo$resumen
#>        columna n_presentes n_fechas_distintas n_duplicados_temporales fecha_minima
#> 1 fecha_evento          11                  9                       2   2024-01-31
#>   fecha_maxima n_fechas_excluidas_parseo                    estado_resumen monotonicidad
#> 1   2024-10-23                         2 calculados_sobre_fechas_parseadas           0.9
#>   cobertura_periodo n_fechas_esperadas_ausentes n_fechas_fuera_calendario n_grupos_huecos
#> 1         0.1111111                           8                         0               1
#>   huecos_truncados
#> 1            FALSE
tiempo$propuestas
#>        columna frecuencia_dias confianza contiguidad cobertura_periodo    calendario
#> 1 fecha_evento              32        NA          NA         0.1111111 1,2,3,4,5,6,7
#>   confirmada                                             evidencia
#> 1      FALSE Moda de intervalos: 32 dias; 8 intervalos observados.
```

## El vocabulario de granularidad

[`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md)
y
[`transiciones_granularidad()`](https://sebollin.github.io/lupa/reference/granularidades.md)
no analizan las tablas. Devuelven el vocabulario de niveles en que se
puede medir y el grafo de agregaciones admitidas.

``` r

granularidades()
#>    nivel           granularidad           relacional implementada
#> 1      1      instanciaAtributo                celda         TRUE
#> 2      2               atributo              columna         TRUE
#> 3      3      conjuntoAtributos conjunto de columnas         TRUE
#> 4      4       instanciaEntidad                tupla         TRUE
#> 5      5                entidad                tabla         TRUE
#> 6      6      conjuntoEntidades   conjunto de tablas         TRUE
#> 7      7              coleccion        base de datos         TRUE
#> 8      8    conjuntoColecciones                 <NA>         TRUE
#> 9      9           organizacion                 <NA>         TRUE
#> 10    10 conjuntoOrganizaciones                 <NA>         TRUE
transiciones_granularidad()
#>              origen                destino                fuente
#> 1 instanciaAtributo               atributo                 marco
#> 2 instanciaAtributo       instanciaEntidad extension_documentada
#> 3  instanciaEntidad                entidad                 marco
#> 4          atributo                entidad                 marco
#> 5           entidad      conjuntoEntidades                 marco
#> 6           entidad              coleccion                 marco
#> 7         coleccion    conjuntoColecciones                 marco
#> 8         coleccion           organizacion                 marco
#> 9      organizacion conjuntoOrganizaciones                 marco
```

Ese vocabulario conecta el examen con el [definición de la
calidad](https://sebollin.github.io/lupa/articles/definir-la-calidad.md):
una estructura observada puede orientar qué medir, pero la métrica, su
granularidad y su interpretación se declaran en el modelo.
