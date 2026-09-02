# lupa <a href="https://sebollin.github.io/lupa/"><img src="man/figures/logo.png" align="right" height="139" alt="sitio de lupa" /></a>

Perfilado y medición de calidad de datos auditables: cada resultado conserva
su alcance y su evidencia.

<!-- badges: start -->
[![Licencia: GPL-3](https://img.shields.io/badge/licencia-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![R-CMD-check](https://github.com/sebollin/lupa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sebollin/lupa/actions/workflows/R-CMD-check.yaml)
[![Estado del repositorio: activo](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![README in English](https://img.shields.io/badge/README-English-1f6feb.svg)](README.md)
<!-- badges: end -->

## Qué problema resuelve `lupa`

`lupa` ayuda a un analista a examinar una tabla o una base antes de confiar en
ella: vuelve visibles las representaciones inconsistentes, los faltantes, las
filas duplicadas o sospechosas, la estructura implícita y los valores que
merecen revisión; después permite convertir reglas confirmadas en mediciones
explícitas, evaluarlas, planificar una limpieza sobre una copia y seguir los
cambios en el tiempo. Es útil cuando un `summary()` no alcanza para saber si
un mismo hecho fue codificado de varias maneras, si un faltante se esconde
detrás de un código o qué filas hay que revisar en el sistema de origen.

## Qué te dice de tus datos que `summary()` no

`summary()` describe las columnas una por una. `lupa` compara
representaciones, cruza columnas y filas, y devuelve `perfil$hallazgos`: una
tabla inspeccionable con tipo de hallazgo, severidad, evidencia, conteos, una
sugerencia y trazabilidad acotada por fila. Un hallazgo en cero significa que
el diagnóstico se ejecutó y no encontró nada; un diagnóstico que no pudo
ejecutarse queda separado en `perfil$cobertura_diagnosticos`, en vez de
informarse en silencio como si los datos estuvieran limpios.

Este es un recorte real de la salida con los datos incluidos:

```r
data(datos_operativos)
perfil <- perfilar(datos_operativos, analizar_dependencias = FALSE)
perfil$hallazgos[, c("columna", "tipo_hallazgo", "severidad", "evidencia")]
```

Entre las salidas de esa corrida aparecen:

| Qué deja visible | `tipo_hallazgo` | Ejemplo en `evidencia` |
| --- | --- | --- |
| Faltantes escondidos detrás de códigos | `faltantes_disfrazados` | `-99 (1)` |
| Fechas en representaciones mixtas | `formatos_fecha_mixtos` | `%d/%m/%Y (4); %Y-%m-%d (4); ...` |
| Espacios sobrantes | `espacios_sobrantes` | `1 valores; ejemplos: "web "` |
| Mayúsculas inconsistentes | `mayusculas_inconsistentes` | `"web"; "Web"` |
| El tipo declarado y el inferido no coinciden | `tipo_declarado_distinto` | `Declarado: texto; inferido: fecha` |
| Una columna constante | `constante` | `Valor: principal; frecuencia: 13` |
| Un valor modal concentrado | `valor_concentrado` | `Valor modal: 1000; frecuencia de la moda: 25; frecuencia del segundo valor: 1; cociente moda/segundo: 25.000; fraccion de la moda sobre validos: 0.250` |
| Filas duplicadas exactas | `filas_duplicadas` | `2 filas en grupos duplicados (1 excedentes)` |
| Columnas repetidas | `columnas_duplicadas` | `id_registro = id_copia` |

En una tabla armada a propósito, el perfil emitió 15 tipos de hallazgo,
incluidas monedas y unidades mezcladas, fechas en formatos mixtos, faltantes
disfrazados, espacios sobrantes, mayúsculas inconsistentes, variantes de
escritura, números guardados como texto, casi-claves, constantes y ausencia
estructural. Sobre un banco de referencia con defectos conocidos recuperó 9 de
9 defectos plantados; sobre 43 tablas limpias produjo 0 hallazgos de severidad
error.

El vocabulario canónico actual contiene 58 nombres de `tipo_hallazgo`. Los
nombres están en español porque forman parte de la API pública:

```text
alta_cardinalidad                 anio_de_dos_digitos
bloqueo_por_con_perdida          casi_clave
casi_duplicados_vocabulario      celdas_multivaluadas
ceros_no_permitidos              clave_con_ausentes
clave_no_unica                   codificacion_invalida
codificacion_rota                columnas_duplicadas
constante                        controles_invisibles
coordenada_fuera_dominio         crs_no_declarado
dato_personal_posible            desviacion_benford
duplicados_aproximados           duplicados_exactos_columnas
duplicados_exactos_normalizados  entidades_html
espacios_sobrantes               faltantes
faltantes_disfrazados            fecha_fallecimiento_fuera_rango
fecha_nacimiento_fuera_rango      fecha_partida_columnas
filas_duplicadas                 formato_fecha_ambiguo
formatos_fecha_mixtos             geometria_invalida
geometria_vacia                  integer64_fuera_precision_double
mayusculas_inconsistentes         monedas_mixtas
negativos_no_permitidos           nombres_columnas_problematicos
normalizacion_unicode             numero_como_texto
outliers                          patron_raro
posible_ausencia_estructural      posible_centinela_numerico
posible_identificador             regla_silencia_ausencia
relacion_aritmetica_columnas      relacion_orden_columnas
separadores_en_campo              tipo_compuesto_no_analizado
tipo_declarado_distinto           tipos_geometria_mixtos
unidades_mixtas                   valor_concentrado
valor_fuera_de_aplicabilidad      valores_no_finitos
variantes_equifrecuentes_vocabulario zona_horaria_fecha_hora
```

La señal `valor_concentrado` implementa la regla M2 medida. Considera sólo
columnas numéricas con al menos 20 valores válidos no faltantes y al menos 10
valores distintos. Emite un hallazgo `sospechoso` cuando la frecuencia de la
moda es al menos cinco veces la del segundo valor más frecuente y el valor
modal representa al menos 0,15 de los valores válidos. La evidencia publica el
valor modal, ambas frecuencias, el cociente y la fracción. Un valor legítimo
puede dominar, por eso nunca es un `error`. Las columnas no elegibles no
reciben una fila de `cobertura_diagnosticos`: las distribuciones categóricas
quedan fuera del alcance previsto de la señal.

Los puntos ciegos medidos son parte de este contrato: no detecta
concentraciones menores al 15 %, y en columnas enteras pequeñas los empates
naturales pueden dejar el cociente por debajo de cinco. La medición encontró
cero falsos positivos en 114 columnas elegibles limpias, con un margen de 2,2
veces.

## Qué NO hace `lupa`

`lupa` no convierte el nombre de un marco de calidad en una medición. Un marco
es una taxonomía; un factor se mide sólo cuando los datos aportan evidencia o
cuando quien usa el paquete declara el requisito y la métrica que lo medirá.
Esta es la cobertura medida de los marcos incluidos:

| marco | factores | alcanzable declarando | fuera de alcance | mide `perfilar()` solo |
| --- | ---: | ---: | ---: | ---: |
| AGESIC | 17 | 12 | 5 | 2 |
| CEPAL | 19 | 6 | 13 | 0 |
| ISO 25012 | 15 | 15 | 0 | 0 |

Los 13 principios de CEPAL fuera de alcance no son una limitación del motor:
hablan del sistema estadístico y de su proceso institucional o productivo, no
de los valores de una tabla. ISO 25012 se cubre entero cuando se declaran sus
requisitos y métricas. `cobertura_analisis()` conserva estas diferencias; que
no haya un hallazgo no afirma que los datos sean buenos.

`lupa` tampoco certifica un conjunto de datos, inventa un puntaje global ni
modifica la entrada como efecto secundario del perfilado. El puntaje sólo
aparece cuando quien usa el paquete declara pesos y un modelo de medición; la
limpieza devuelve una copia y conserva intactos los datos originales.

## Sobre qué bases corre

Hoy se midieron siete combinaciones de motor y versión contra motores reales.
Cada una produjo las 38 métricas solicitadas, declaró 7 como `no_aplica` y
detectó la clave `id` en la tabla de prueba.

| motor | versión | estado |
| --- | --- | --- |
| PostgreSQL | 16 | **medido contra el motor real** |
| PostgreSQL | 9.3.25 | **medido contra el motor real** |
| MariaDB | 11.8 | **medido contra el motor real** |
| MySQL | 8.4 | **medido contra el motor real** |
| SQLite | — | **medido contra el motor real** |
| DuckDB | — | **medido contra el motor real** |
| SQL Server | 2022 | **medido contra el motor real** |
| Oracle | — | no medido en esta matriz |
| BigQuery | — | no medido en esta matriz |

`requisitos_motor()` tiene 12 entradas. Nueve son entradas de motores con
nombre si se cuentan por separado las dos filas de versión de Oracle;
`dbi`, `odbc` y `otro_dbi` son entradas genéricas de compatibilidad, no tres
motores medidos adicionales.

## Cómo se empieza

Hasta la primera publicación en CRAN, instalá la versión de desarrollo desde
GitHub:

```r
pak::pak("sebollin/lupa")
```

Después, estas cinco líneas permiten mirar una tabla y su cobertura declarada:

```r
library(lupa)
data(datos_operativos)
perfil <- perfilar(datos_operativos, analizar_dependencias = FALSE)
head(perfil$hallazgos[, c("columna", "tipo_hallazgo", "severidad")], 5)
cobertura_analisis(perfil)
```

`perfilar()` sólo lee. El paso siguiente, cuando un hallazgo se confirma como
requisito, está recorrido en la viñeta
[`flujo-guiado`](https://sebollin.github.io/lupa/articles/flujo-guiado.html).

## 🔎 Cómo trabaja lupa en la práctica

`lupa` es un conjunto de herramientas auditables para conectar el primer
perfilado con un modelo de calidad declarado para un uso concreto, mediciones
explícitas, limpieza controlada de una copia y duplicados aproximados a escala.
En vez de un puntaje opaco, cada resultado conserva alcance, evidencia e
incertidumbre.

**Bases enteras, no una tabla sola.** `coleccion()` declara qué tablas componen
una base —con su esquema, porque el esquema es parte de la identidad de la
tabla— y `perfilar_coleccion()` devuelve una fila por tabla más la cobertura de
lo que no pudo medir. La frontera se *declara*, nunca se descubre: recorrer un
catálogo convertiría un error de permisos en un resultado, y una colección real
pasa de mil tablas repartidas en decenas de esquemas. Lo que una credencial no
puede leer va a `cobertura_coleccion` con su motivo, **nunca a cero** —los
permisos parciales son el caso normal, no el borde—. Y no hay lectura
instantánea: cada tabla trae el momento en que se midió, y el objeto lo declara.

**Contradicciones que ninguna columna muestra sola.** Con `senal_redundante()`
se declara que varias columnas codifican el mismo hecho, y
`detectar_discordancias()` informa las filas donde no concuerdan: el año de la
fecha contra el año fiscal contra el año del archivo. Cada uno de los tres puede
ser plausible por su cuenta y aun así contradecir a los otros. El grupo se
declara, nunca se adivina: dos columnas de año pueden ser el de nacimiento y el
de ingreso, y no tienen por qué coincidir.

**Hallazgos que se pueden verificar, no sólo leer.** Pasale `clave` a
`perfilar()` con las columnas que identifican una fila, y la trazabilidad de
cada hallazgo trae esos valores para las filas que señala, así el caso se busca
en el sistema de origen sin abrir la tabla. El índice de fila queda como
respaldo, y `trazabilidad$localizador` dice cuál de los dos te tocó. Hay una
tensión que el rasgo no puede ignorar: **la clave que permite ir a verificar es
exactamente lo que identifica a una persona**, así que una columna de la clave
clasificada como dato personal vuelve enmascarada, igual que la evidencia, y
`claves_protegidas` dice cuál.

**El perfilado no toca los datos.** Ninguna función de análisis altera la tabla
que recibe —ni sus valores, ni sus tipos, ni sus nombres, ni sus atributos—,
incluidos los `data.table`, que R permite modificar por referencia. La única
capa que produce datos distintos es la de remediación, y devuelve una copia: la
tabla que pasaste sigue siendo la que tenés. Hay una prueba de regresión que lo
verifica en cada punto de entrada.

## 🌎 Idioma de la API

Los nombres públicos están en español en ejemplos, ayuda y viñetas:

| API en español | Significado en inglés |
| --- | --- |
| `perfilar()` | profile |
| `analizar()` | analyse |
| `marco_calidad()` | quality framework |
| `planificar_limpieza()` | plan a cleanup |
| `guiar_limpieza()` | guide a cleanup |
| `aplicar()` | apply a selected cleanup |
| `medir()` / `tablero_calidad()` / `evaluar()` | measure / dashboard / evaluate |
| `detectar_duplicados_aproximados()` | find approximate duplicates |
| `reportar()` | create a report |

El [README en inglés](README.md) cuenta lo mismo. Quienes contribuyan deben
mantener el contrato público en español; la guía que lo rodea sí se puede
internacionalizar.

## ⚡ Un análisis completo y un informe

Hasta la primera publicación en CRAN, instalá la versión de desarrollo desde
GitHub:

~~~r
# install.packages("pak")
pak::pak("sebollin/lupa")
~~~

Después perfilá una tabla o ejecutá el recorrido integral:

~~~r
library(lupa)
data(datos_operativos)

perfil <- perfilar(datos_operativos, analizar_dependencias = FALSE)
head(perfil$hallazgos[, c("columna", "tipo_hallazgo", "severidad")], 5)

analisis <- analizar(datos_operativos)
analisis$tablero
archivo <- tempfile(fileext = ".html")
reportar(analisis, archivo = archivo)
stopifnot(file.exists(archivo))
unlink(archivo)
~~~

El perfilado es de sólo lectura: nunca cambia la tabla de entrada. Los
hallazgos son data frames inspeccionables y la evidencia de datos personales se
enmascara cuando la clasificación lo justifica. Abajo se ve una salida real de
consola:

![Salida capturada de `perfilar()`](man/figures/perfil-console.png)

## 🧭 Qué se puede hacer con lupa

La [referencia de pkgdown](https://sebollin.github.io/lupa/reference/) y las
viñetas enlazadas son el manual detallado. Esta tabla es el mapa breve:

| Tarea | Funciones principales | Para leer más |
| --- | --- | --- |
| Mirar los datos por primera vez | `perfilar()`, `analizar()`, `distribucion_valores()`, `detectar_asociaciones()`, `analizar_tiempo()`, `clasificar_variables()`, `inferir_tipo()`, `descubrir_patrones()`, `detectar_formatos_fecha()`, `sentinelas_naniar` | [Empezar con lupa](https://sebollin.github.io/lupa/articles/empezar-con-lupa.html) |
| Perfilar contra una base | `perfilar_dbi()` — agregados SQL de toda la tabla y, por omisión, un perfil de 109 campos analíticos sobre una muestra declarada; `bloque_muestra = "solo_agregados"` permite pedir sólo los agregados | [Perfilar una base](https://sebollin.github.io/lupa/articles/perfilar-una-base.html) |
| Encontrar estructura no declarada | `detectar_claves()`, `detectar_relaciones()`, `detectar_dependencias()`, `granularidades()`, `transiciones_granularidad()` | [Estructura no declarada](https://sebollin.github.io/lupa/articles/estructura-no-declarada.html) |
| Definir la calidad | `marco_calidad()`, `marco_agesic()`, `marco_iso25012()`, `marco_cepal()`, `catalogo_agesic()`, `metrica()`, `especializar()`, `instanciar()`, `modelo()`, `metricas_nucleo()`, `metricas_referencial()`, `proponer_modelo()`, `modelo_desde_propuesta()`, `perfiles_madurez()`, `cobertura_analisis()` | [Definir la calidad](https://sebollin.github.io/lupa/articles/definir-la-calidad.html) |
| Medir y evaluar | `medir()`, `agregar()`, `tablero_calidad()`, `indice_calidad()` con pesos del proyecto, `evaluar()`, `regla_evaluacion()` con la instrucción `desenlace = "suprimir"` declarada por quien usa el paquete (no un umbral de fábrica), `perfil_evaluacion()`, `escala()`, `referencial()`, `vigencia()` | [Medir y evaluar](https://sebollin.github.io/lupa/articles/medir-y-evaluar.html) |
| Limpiar sin romper nada | `planificar_limpieza()`, `guiar_limpieza()`, `aplicar()` | [Plan de limpieza](https://sebollin.github.io/lupa/articles/limpiar-con-un-plan.html) |
| Encontrar duplicados aproximados | `detectar_duplicados_aproximados()`, `estimar_costo()` | [Escala y duplicados](https://sebollin.github.io/lupa/articles/escala-y-duplicados.html) |
| Reparar codificación dañada | `reparar_codificacion` mediante `planificar_limpieza()` y `aplicar()` | [Referencia de limpieza](https://sebollin.github.io/lupa/reference/planificar_limpieza.html) |
| Seguir la calidad en el tiempo | `historico_calidad()`, `acumular_historico()`, `guardar_historico()`, `leer_historico()`, `detectar_deriva_calidad()`, `comparar_perfiles()`, `comparar_equivalencia()`, `comparar_evaluaciones()` | [Histórico y deriva](https://sebollin.github.io/lupa/articles/historico-y-deriva.html) |
| Compartir resultados | `reportar()`, `guardar_analisis()`, `leer_analisis()` | [Referencia de informes](https://sebollin.github.io/lupa/reference/reportar.html) |
| Validar y extender | `validadores_internacionales()`, `validadores_uruguay()`, `pack_validadores()`, `validar_ci_uy()`, `validar_rut_uy()`, `validar_luhn()`, `validar_mod97()`, `validar_iso3166()`, `validar_iso4217()`, `validar_correo()`, `validar_url()` | [Referencia](https://sebollin.github.io/lupa/reference/) |

## 📐 Alcance: qué se mide sobre todo y qué se muestrea

`perfilar()` usa todas las filas para los conteos de tabla y columna, faltantes
reales y disfrazados, valores distintos, duplicados exactos, resúmenes
cuantitativos y los hallazgos derivados de esas cantidades. Por omisión,
`muestra = 1e5` limita el descubrimiento de patrones, la inferencia de tipos, la
detección de formatos de fecha y la muestra común con que se buscan dependencias
funcionales. Otro límite o `Inf` cambia o desactiva ese muestreo.

Para tipos temporales inferidos, `estado_tipo_inferido` distingue `confirmado`,
`candidato` y `NA`; una fecha ambigua compatible al 100 % sigue siendo candidata.

Los validadores de documentos personales tienen otro filtro preliminar:
`muestra_validadores = 1000` por omisión. Si un validador supera ese filtro se
evalúa luego sobre toda la columna; `Inf` vuelve completo incluso el primer
paso. Los duplicados aproximados están apagados por omisión y, cuando se
activan, tienen sus propios límites declarados.

En `detectar_duplicados_aproximados()`, `pares$tipo_par` se describe solo:
`exacto` significa que los textos guardados son iguales,
`exacto_normalizado` que sólo coinciden después de la normalización declarada,
y `aproximado` que siguen siendo similares. `pares$igualo_normalizar` marca el
caso intermedio. Los conteos correspondientes en el alcance son
`n_pares_exactos`, `n_pares_exactos_normalizados` y `n_pares_aproximados`.

El resultado deja el alcance efectivo en `meta$muestra`,
`meta$filas_analizadas` y `meta$muestreo`; cada columna también publica
`n_filas_analizadas_tipo` y `muestreado_tipo_inferido`, y la tabla de
dependencias conserva atributos con las filas analizadas y el muestreo.
`analizar()` reutiliza `muestra = 1e5` para su perfil, distribuciones y niveles
observados, y declara límites separados para asociaciones y los demás
componentes.

## 🗄️ Motores: qué está probado y qué está esperado

`perfilar_dbi()` no promete un dialecto universal. Resuelve el dialecto con una
sonda de cero filas **antes** de emitir el bloque de agregados, y lo que el motor
rechaza queda declarado como no disponible con su motivo, nunca en cero.

| motor | dialecto | estado |
| --- | --- | --- |
| **SQLite** | `limit` | **probado** contra el motor real: 38 métricas, 7 `no_aplica` y clave `id` detectada |
| motor que rechaza `LIMIT` | `top` / `portable` | **probado** con un motor simulado en la suite |
| motor que pliega los alias a mayúsculas | cualquiera | **probado** con un motor simulado |
| motor que rechaza `SELECT *` por una columna | cualquiera | **probado** con un motor simulado |
| **PostgreSQL 16** | `limit` | **probado** contra el motor real: 38 métricas, 7 `no_aplica` y clave `id` detectada |
| **PostgreSQL 9.3.25** | `limit` | **probado** contra el motor real: 38 métricas, 7 `no_aplica` y clave `id` detectada |
| **MySQL 8.4** | `limit` | **probado** contra el motor real: 38 métricas, 7 `no_aplica` y clave `id` detectada |
| **SQL Server 2022** | `top` | **probado** contra el motor real: 38 métricas, 7 `no_aplica` y clave `id` detectada |
| **DuckDB** | `limit` | **probado** contra el motor real: 38 métricas, 7 `no_aplica` y clave `id` detectada |
| **MariaDB 11.8** | `limit` | **probado** contra el motor real: 38 métricas, 7 `no_aplica` y clave `id` detectada |
| **Oracle** | `fetch_first` / `rownum` | no medido en la matriz actual |
| **BigQuery** | `portable` | no medido en la matriz actual |
| cualquier otro compatible con DBI | `portable` | reserva: `dbSendQuery()` + `dbFetch(n)` |

La afirmación es reproducible: `benchmark/verificar_motor.R` toma cualquier
conexión DBI y comprueba cinco cosas — que el perfil tenga cinco columnas, que
el dialecto se resuelva por sonda, que la media del motor coincida con R, que la
clave primaria se lea del catálogo y que la cobertura sea una tabla.

Lo que ese script comprueba es el **comportamiento**, y se puede rehacer contra
cualquier conexión. Los **cronometrajes** de esas corridas —los segundos y las
lecturas de fila que aparecen en las notas de versión— no se rehacen desde el
repositorio: necesitan la infraestructura de la corrida, hasta dos millones de
filas por cuarenta columnas sobre un motor levantado para la ocasión. Están
publicados como lo que son, referencias de una corrida puntual, y ningún
resultado del paquete depende de ellos.

**Esperado** significa que el dialecto está construido y probado contra un motor
simulado que reproduce esa restricción, no que se haya corrido contra el motor
real. La diferencia importa y por eso está escrita: los defectos que esta versión
corrigió no aparecieron en ocho entornos verdes justamente porque todos usaban el
mismo motor.

Cada motor que se agregó a esta tabla encontró un defecto que ningún motor
simulado podía encontrar. El de DuckDB es el más filoso: acepta
`TABLESAMPLE SYSTEM (10) WHERE 1 = 0` y rechaza la misma cláusula sin el filtro,
porque con un filtro trivialmente falso su parser no llega a validar el método
de muestreo. La sonda de capacidad usaba justo ese filtro para salir barata, así
que pasaba, y la consulta real fallaba. **Una sonda que no ejercita la forma que
después se emite no prueba nada** — la misma lección que había dejado la sonda
del desvío una ronda antes.

El dialecto se puede declarar con `dialecto =` si la sonda no acierta. Un fallo
parcial nunca descarta lo ya medido: si la lectura de la muestra falla, el objeto
vuelve con `resumen_tabla` completo, `perfil_muestra = NULL` y una fila de
cobertura con el motivo. Si no se pidió la muestra, la cobertura usa
`no_solicitado`, que no es un fallo; se puede pedir sólo los agregados con
`bloque_muestra = "solo_agregados"`.

### Saber qué falta antes de chocarse

`lupa` tiene dos dependencias obligatorias, `cli` y `data.table`. `data.table` se
usa únicamente para acelerar el conteo exacto de filas duplicadas, y nunca se
importa al espacio de nombres; las tablas con columnas de lista o matriz, o con
`NaN`, repliegan a la implementación de base, que es la que fija el resultado. Todo lo
demás es opcional —y lo que pasaba cuando faltaba algo era un error de R, o del controlador, que
no decía ni qué faltaba ni cómo conseguirlo. El caso duro no es el paquete de R
sino la **biblioteca del sistema que va debajo**: `RMariaDB` no compila sin las
cabeceras del cliente de MySQL o MariaDB, `ROracle` necesita el Instant Client
de Oracle, y quien ve `installation of package 'RMariaDB' had non-zero exit
status` no tiene forma de saber que la respuesta es `libmariadb-dev`.

```r
requisitos_motor()            # el catálogo entero
requisitos_motor("oracle")    # qué necesita Oracle, y cómo conseguirlo
```

Por cada motor declara el paquete de R, la biblioteca del sistema con su nombre
en Debian y en Fedora, **la salida sin permisos de administrador** cuando
existe, el dialecto esperado y si está probado contra motor real. Las salidas no
son hipotéticas: para SQL Server no había driver ODBC ni forma de instalarlo, y
se resolvió compilando FreeTDS en un prefijo del usuario y pasándole la ruta a
`odbc`; el Instant Client de Oracle se descomprime en una carpeta propia.
Ninguna de las dos necesitó `sudo`.

Lo que **no** hace es afirmar que comprobó una biblioteca del sistema que no
puede comprobar. Cuando sólo puede decir «el paquete de R no está, y si al
instalarlo falla la compilación lo que falta es esto», dice exactamente eso: el
invariante del paquete aplicado a su propia instalación.

### Una consulta por lote, no una por columna

El perfilado emitía **una consulta por columna** para cada bloque de métricas.
Sobre una tabla de decenas de millones de filas ése es el costo: no el muestreo,
la cantidad de escaneos. Los agregados planos —`COUNT(col)`, mínimo/máximo/
media/ceros/negativos y desvío— se piden ahora para **varias columnas en una
sola consulta**, por lotes. `COUNT(DISTINCT ...)` conserva una clase separada;
la moda sigue siendo una por columna porque agrupa. La mediana conserva una por
columna como respaldo; cuando la sonda del motor acepta
`PERCENTILE_CONT(...) WITHIN GROUP`, varias medianas viajan en un solo
`SELECT` por lote. En PostgreSQL 9.3 y SQLite, donde esa funcion no esta
disponible, la mediana por columna conserva `LIMIT` pero calcula el conteo como
subconsulta escalar de la misma sentencia. La sonda comprueba tambien la
division entera (`%` y `/`); si el dialecto no admite esa forma, la salida
declara que conserva las dos consultas.

La mediana consolidada tambien se sondea antes de usarla; en SQL Server
requiere nivel de compatibilidad >= 110. Se sondea; estos son los motivos
conocidos de que no se active: la funcion no esta disponible o el motor rechaza
la sonda. El mensaje del motor queda en
`meta$mediana_consolidada$motivo`, y se conserva la mediana por columna.

La procedencia de la cardinalidad se elige con `estrategia_distintos`, cuyo
valor por omisión es `"exacta"`. `"exacta"` emite `COUNT(DISTINCT)` sobre las
filas de la corrida; `"aproximada_motor"` usa una función nativa sólo si la
sonda la acepta; `"catalogo"` lee `pg_stats.n_distinct` en PostgreSQL, lo
publica como estimación y aplica guardas de herencia y del universo muestreado; en
otros motores queda `no_disponible` con su motivo; y `"omitida"` no emite la
consulta. No hay repliegue silencioso entre estrategias: una aproximación sin
capacidad queda `no_disponible`, no se convierte en un conteo exacto.

La salida conserva `estrategia_solicitada`, `estrategia_resuelta` y `estado` en
`resumen_tabla$meta$estrategia_distintos` y en las filas de `sql`. En una
aproximación nativa, las cotas de error no documentadas quedan como
`"desconocido"`; si la capacidad sólo construye una consulta completa, válidos
y distintos se emiten por separado y cada registro conserva el método de la
consulta que efectivamente se ejecutó.

Medido contra PostgreSQL 16 con **2 millones de filas por 40 columnas**:

| configuración | antes | después |
| --- | --- | --- |
| `metricas = "validos"` | 46 consultas, 5,4 s | **8 consultas, 2,4 s** |
| `metricas = c("validos", "basicos", "desvio")` | 128 consultas, 15,2 s | 14 consultas, 5,3 s; **10 consultas** con la fusión plana |

Con las mismas 160 y 400 métricas calculadas, y con los mismos números: sobre
una tabla sembrada una sola vez, el perfil consolidado y el anterior coinciden
en los dieciséis campos del resumen para seis tipos de columna.

**Si un lote falla, no se pierde el lote.** Se sondean sus mitades por bisección:
los grupos aceptados se reutilizan como mediciones y las columnas culpables se
reintentan por métrica. Lo que igual falle queda `no_disponible` con su motivo,
mientras las vecinas se calculan. Si el presupuesto se agota antes de aislar
todo, las columnas pendientes quedan sin medir; no se las supone culpables ni
legibles. La degradación tiene sus propios tests.

`resumen_tabla$sql` conserva **una fila por columna y métrica** con todos sus
campos, y agrega `lote` y `columnas_compartidas` para que se vea cuál consulta
fue compartida. También agrega `consulta_id`, que identifica la sentencia que
produjo cada medición y define el grupo de consistencia comprobable. El campo
heredado de consulta ahora se llama `id_consulta`, sin alias: identifica la
consulta de datos. `muestra_id` queda reservado para la relación materializada
y se publica en `meta$materializacion`, nunca como segundo nombre del id de
consulta.

Con `universo = "muestra_motor"`, la selección del motor se materializa una
sola vez en un spool externo de la sesión cliente. Su trailer verifica
`muestra_id`, `snapshot_id`, `orden_id`, `n_filas`, `bytes` y checksum en la
relectura. Todo el perfil lee ese spool; nunca vuelve a muestrear el motor. Cada
chunk se compara con `max_bytes_materializacion` antes de escribir y un exceso
publica `spool_presupuesto_excedido` junto con
`muestra_inestable:presupuesto_materializacion`, sin resultado híbrido. El spool
no escribe en la conexión DBI ni crea objetos temporales del motor.
`meta$materializacion` conserva backend, versión, checksum, bytes y presupuesto.

El punto de cruce medido se declara como costo, no como promesa de velocidad:
en PostgreSQL 16 y 2 millones de filas, una muestra de 500.000 tardó unos
5,6 s con spool frente a 3,3 s reordenando en cada pasada; se elige spool por
identidad y reutilización acotada. Para 10.000, 100.000 y 500.000 filas, los
totales con spool fueron 0,448, 1,684 y 5,598 s, frente a 0,814, 2,178 y 3,265
s reordenando (cruce entre 100.000 y 500.000).

La misma auditoría SQL incluye `memoria_trabajo`: `creciente`, `acotado` o `NA`,
para señalar qué trabajo no conviene recalcular incrementalmente sobre una tabla
mayor.

Con `instrumentar = TRUE`, cada consulta suma `duracion_ms` y
`cpu_ms`. El primero usa `Sys.time()`; el segundo suma
`proc.time()[c("user.self", "sys.self")]` y representa el trabajo de CPU
del cliente, incluida la conversión del resultado que hace el driver. Cero es
medido; `NA` significa que no se pudo medir. `instrumentar = FALSE` apaga
ambos relojes y deja esos campos en `NA`, sin alterar la consulta ni su
resultado.

Cada consulta de agregados planos que trae `n_validos` incluye también
`COUNT(*) AS n_total_consulta` en la misma sentencia. La completitud usa ese
denominador local, incluso después de una bisección; no cruza el total de otro
lote. El total del universo se conserva por separado cuando el perfil se
calcula sobre una muestra o cuando no hay un agregado que pueda llevarlo. La
consulta exacta de distintos incluye `COUNT(columna) AS n_validos_guard` junto
a `COUNT(DISTINCT columna)`: la cota dura sólo se aplica si ambos valores tienen
el mismo `consulta_id`. Si una capacidad aproximada no puede traer ese guardián,
la comprobación queda declarada como no disponible y no se atribuye una
inconsistencia al motor. Los tamaños se pueden separar:
`tamano_lote_planos` controla los agregados planos y `tamano_lote_distintos` las
cardinalidades; el primero vale 20 y el segundo 2. La asimetría está medida: una
sola cardinalidad puede derramar mucho más que un lote plano, pero un lote de 1
resultó peor que uno de 2 —27,6 segundos por columna contra unos 15—, así que el
valor por omisión salió de la medición y no de la precaución.
La consulta de la moda intenta traer `SUM(COUNT(*)) OVER () AS n_validos_guard`
junto a su frecuencia. La forma se sondea antes de usarla; si el motor la
rechaza, conserva la consulta anterior y lo declara en
`resumen_tabla$meta$moda_guardian`. Cuando la acepta, la cota
`frecuencia_moda <= n_validos` se comprueba dentro de esa misma sentencia.

El catálogo de la clave primaria se consulta en todas las corridas, aunque la
política de costo no necesite la cardinalidad. La respuesta queda publicada en
`resumen_tabla$meta$clave`, con `columnas`, `fuente`, `motivo`, `garantia` y
`estado`. La consulta es de metadatos y no recorre la tabla. Una clave visible
sin garantía suficiente conserva `garantia = "desconocida"`; una tabla sin
clave declarada conserva `"no_declarada"`, y una consulta fallida conserva su
motivo sin convertirlo en ausencia de clave.

Las fuentes estructurales que usa la política de costo se resuelven cuando la
política necesita la cardinalidad, aunque la estrategia solicitada esté omitida
o no disponible. La disponibilidad gobierna si se puede medir la cardinalidad;
no oculta una clave primaria garantizada. Si no hay fuente estructural y la
estrategia no permite medir, la cardinalidad queda desconocida y la política
sigue su regla explícita para ese caso.

El resumen de tabla publica `meta$snapshot = FALSE`: los agregados son
sentencias separadas y la tabla puede cambiar entre ellas. Si `n_validos` y
`n_distintos` exactos son incoherentes, provienen de grupos `consulta_id`
distintos y se pueden comparar sólo dentro de cada sentencia, `cobertura` suma
`alcance_distinto` con ambas sentencias en el motivo. Eso es evidencia de que la
tabla cambió durante la corrida, no una acusación contra el motor o el paquete.

### Leer un perfil sin conocer su forma

`perfilar()` devuelve un `perfil` plano; `perfilar_dbi()` devuelve un
contenedor. Así que `perfil$general$filas` funcionaba sobre uno y devolvía
`NULL` sobre el otro, donde el conteo vive en `resumen_tabla$meta$filas`. Un
`NULL` silencioso en un guion de medición es la peor forma de fallar: no avisa,
y lo que sigue calcula sobre nada.

```r
hallazgos(x)    columnas(x)    cobertura(x)    n_filas(x)    sql_perfil(x)
```

Andan sobre `perfil`, `analisis`, `perfil_dbi` y `perfil_coleccion`, y no
inventan lo que no hay: un perfil DBI sin muestra leída devuelve una tabla de
hallazgos vacía **con su aviso**, y `sql_perfil()` sobre un perfil en memoria
devuelve `NULL`, porque una tabla sin filas sugeriría que se emitió SQL y no
encontró nada.

### Un presupuesto mide trabajo, no cuenta unidades

Un tope que cuenta unidades trata igual a una columna de códigos de diez
caracteres y a una de WKT de mil. Una tabla del catálogo de PostGIS —3.912
filas— tardaba 243 segundos, y el detector de vocabulario era el 99,6 %: 800
valores distintos son 319.600 pares, muy por debajo del tope de dos millones,
pero cada comparación era una Jaro-Winkler sobre 900 caracteres.

El presupuesto se mide ahora en **comparaciones de carácter**, que es el bucle
interno de la distancia. Calibrado contra la medición, la columna patológica
baja de 61,3 s a 4,6 s. Una columna corriente de dos mil valores se compara
entera **mientras sus valores midan menos de unos cien caracteres**: el
presupuesto muerde cuando `L² · n(n−1)/2` supera `2e10`, que para dos mil valores
distintos es un largo de 101. Decir «dos mil valores se comparan enteros» sin esa
condición era falso, y falso en el peor lugar: la columna de WKT de 900
caracteres que motivó el presupuesto es justo de las que sí se recortan. Lo que se recorta se declara: cuántas formas quedaron sin
comparar, cuánto trabajo eran y cuál de los topes recortó.

### Un tope de largo protege la señal de la distancia

La distancia de edición normalizada está pensada para nombres, direcciones e
identificadores, no para documentos. Se midieron cinco semillas con pares de
textos aleatorios: uno difería en un carácter y el otro en 1.000. La tabla
muestra la mediana de las cinco semillas. El segundo
par dejó de parecer distinto al umbral predeterminado `0,10` antes de que el
primero dejara de ser una variante cercana:

| largo | distancia, 1 carácter | distancia, 1.000 caracteres |
| ---: | ---: | ---: |
| 10.000 | 0,000400 | 0,193195 |
| 20.000 | 0,000150 | 0,125923 |
| 25.000 | 0,000347 | 0,104511 |
| 50.000 | 0,000053 | 0,064559 |

El cruce observado está entre 25.000 y 50.000 caracteres. Se eligió `10.000`
como tope predeterminado para dejar un margen de cinco veces antes de esa
perdida de discriminacion. `detectar_duplicados_aproximados()` lo publica en
`alcance$max_largo_valor`; si alguna columna supera el tope, la combinacion
completa queda fuera de alcance y `alcance$columnas_excluidas_largo` explica
por que. En `perfilar()`, la misma decision aparece en
`cobertura_diagnosticos` con el motivo, el largo observado y el umbral.
`max_largo_valor = Inf` o `max_largo_valor_vocabulario = Inf` recupera
explicitamente el comportamiento anterior.

El tope se mide sobre la cadena que **de verdad se compara**: las columnas ya
combinadas y ya normalizadas. Las dos mitades de esa frase importan. Dos columnas
de nueve mil caracteres cada una están por debajo del tope por separado y llegan
a dieciocho mil una vez unidas; y la normalización `amplio` expande ligaduras
—la de f-f-l es un solo carácter que se convierte en tres—, así que un valor puede
quedar por debajo del tope guardado y por encima del comparado.
`alcance$largo_maximo` publica ese largo comparado, y vale `NA` —no cero— cuando
el tope no aplica y no se midió ningún largo.

Los dos topes son perillas separadas a propósito. `max_largo_valor_vocabulario`
gobierna la regla de vocabulario dentro de `perfilar()`; el detector de pares de
filas conserva el suyo, que se configura con
`duplicados_aproximados = list(max_largo_valor = ...)`.

### Avisos de costo y derrame

Cuando `perfilar_dbi()` pide `COUNT(DISTINCT ...)`, los agregados planos se
ejecutan primero cuando fueron solicitados, pero no son una referencia temporal
para los distintos. Con `instrumentar = TRUE`, el paquete mide el primer lote
de distintos en esta misma corrida y, si queda otro lote, anuncia la
proyeccion despues del primero y antes del segundo. El numero es la mediana de
las duraciones de las consultas del primer lote de distintos multiplicada por
la cantidad de lotes; el mensaje nombra esa fuente y lo rotula como estimacion.
Con un solo lote no queda trabajo que evitar, asi que no se publica una
proyeccion. Por eso una peticion con `metricas = "distintos"` tambien recibe el
aviso cuando hay varios lotes. Esta proyeccion temporal no usa `reltuples`.

El mismo canal avisa ahora el costo de las dos metricas caras que faltaban:
`moda` se proyecta por la suma de las cardinalidades de las columnas y
`mediana` por la cantidad de filas. Cada una tiene su interruptor y su umbral
en segundos, encendidos por omision a partir de 30 segundos:
`avisar_costo_moda`/`umbral_segundos_aviso_moda` y
`avisar_costo_mediana`/`umbral_segundos_aviso_mediana`. El aviso se emite antes
de la consulta que se proyecta y queda en `meta$costo_moda` o
`meta$costo_mediana`, separados de `meta$costo_distintos` porque cada
proyeccion usa una unidad distinta.

Cuando puede, la moda mide en esta corrida la primera consulta y usa sus ms
por distinto para las columnas restantes. Si no hay cardinalidad exacta, usa
la fuente disponible (por ejemplo, catalogo) y la declara; si no hay ninguna,
la proyeccion queda no disponible. La mediana conoce las filas despues del
primer conteo. La primera mediana medida sirve como referencia local para las
restantes; si una sola mediana total no deja medicion local, usa la referencia
de banco declarada de 68 ms por millon de filas, tomada de otra corrida. Esa
referencia no se presenta como medicion de la corrida actual.
Si la consulta inicial que obtuvo las filas fue medida y da una cota mayor,
esa cota se publica aparte como lectura observada —no como medicion de mediana—
para evitar el falso silencio de una tabla grande recién cargada.

En PostgreSQL, la preparacion consulta ademas `pg_stats.n_distinct`,
`pg_stats.avg_width`, `pg_class.reltuples` y la configuracion vigente de
`work_mem` —mas `hash_mem_multiplier` desde PostgreSQL 13— para estimar el
tamano del hash de agregacion. Si supera el limite efectivo, avisa antes del
primer `COUNT(DISTINCT)` y explica que subir `work_mem` en la sesion puede
evitar el derrame. El diagnostico queda en `meta$estimacion_derrame`, siempre
como estimacion y nunca como medicion; no cambia la configuracion y no espera
confirmacion.

En PostgreSQL, si `pg_stat_statements` permite atribuir exactamente una llamada
de esta corrida, el informe agrega el derrame real y la cantidad de bloques
temporales escritos. Si no se puede atribuir, el estado queda declarado como
no disponible: el tiempo transcurrido no se presenta como evidencia de derrame.
Cuando existe, esa medicion **manda sobre la estimacion**: aunque la estimacion
haya quedado por debajo del limite, el informe dice que hubo derrame medido.

### Duraciones SQL con una unidad que se puede sumar

`resumen_tabla$sql` tiene una fila por columna y metrica, pero `duracion_ms` es
la duracion de la consulta y se repite en cada fila que produjo esa consulta.
La tabla ahora incluye `nivel`, siguiendo a `resumen_tabla$tiempos`: la primera
fila de cada `consulta_id` queda en `nivel = 1` y las repetidas en `nivel = 2`.
Sumá `duracion_ms` sólo para `nivel = 1` (y usá `na.rm = TRUE` cuando
corresponda); sumar la columna entera cuenta más de una vez las consultas
compartidas. Las filas sin consulta conservan la duración en `NA` y no afirman
una medición.

### El costo de una tabla ancha se avisa antes

`perfilar()` proyecta las celdas antes de empezar el trabajo costoso. El aviso
predeterminado se activa desde `100.000` celdas, usa una referencia de `10.000`
celdas por segundo y publica que es una estimacion junto con su fuente. La
referencia reproduce estas mediciones (corrida del 2026-08-30, reproducible con
`benchmark/medir_referencias.R`): 500 filas por 50, 300 y 1.000 columnas tomaron
3,34, 13,63 y 43,85 segundos — unas 11.000 celdas por segundo, y por eso la
referencia usa 10.000. No hay aviso por debajo del umbral ni en
guiones no interactivos. La proyeccion queda en `meta$costo_tabla_ancha`.
`avisar_costo_tabla_ancha = FALSE` lo desactiva por llamada y
`umbral_celdas_aviso_tabla_ancha = Inf` lo silencia de forma explicita.

Y cuando hay que recortar, las formas que quedan son las **primeras del
alfabeto**, no las primeras en aparecer. Esa diferencia era un defecto, medido
sobre una columna real: 45.400 nombres de calle del catálogo nacional de datos
abiertos, 8.318 formas distintas. Las mismas filas daban 26 grupos de
casi-duplicados en el orden en que viene el archivo, 70–85 desordenadas y 148
ordenadas. Un perfilador cuyo veredicto depende del orden de las filas está
midiendo la forma física de la tabla y no los datos. Ordenando antes, los cinco
órdenes dan 148 —y ordenar además deja los casi-duplicados adyacentes, así que el
corte cae entre familias en vez de partirlas—.

**El otro recorte, el de pares, sigue la misma regla desde la misma medición.**
`max_resultados` conserva los pares más cercanos ordenando por distancia, y entre
pares **empatados** desempataba por posición de fila. Medido sobre 60 grupos cuyos
pares internos comparten exactamente la misma distancia, con el corte en 30, cinco
órdenes distintos devolvían 30 grupos cada uno **sin compartir ninguno**. Ahora
desempata por el orden canónico de los valores, con una clave simétrica para que
tampoco dependa de cuál fila quedó primera dentro del par, y los cinco órdenes
devuelven exactamente los mismos grupos.

Lo que ningún orden arregla es que un corte dentro de un empate deja afuera pares
igual de cercanos. Eso no se arregla: se declara. `alcance` trae
`distancia_corte`, `n_en_distancia_corte` y `corte_en_empate`, y ese último no es
`truncado` con otro nombre —da `FALSE` cuando el corte cae en una distancia
única—.

### Los topes de tamaño protegen las lecturas remotas

`perfilar()` y `perfilar_dbi()` usan los mismos topes predeterminados de la
muestra: `max_celdas_muestra = 1.000.000` celdas y
`max_bytes_muestra = 512 MiB`. En los perfiles DBI sólo alcanzan a
`perfil_muestra`; los agregados SQL conservan el alcance que les corresponde.
El tope de celdas se resuelve con el conteo de filas y el ancho del esquema,
antes de leer. El de bytes hace primero una sonda de hasta 100 filas y luego
pone el límite resultante en el SQL final o en `dbFetch(n)`: la muestra completa
no se trae para recortarla después en R.

Si `muestra = n` y un tope es más estricto, manda el menor. La cobertura del
perfil declara las celdas o bytes observados, el umbral y el motivo, incluido
cuál tope ganó. Con `Inf` en ambos topes no se declara ningún recorte.
`plan_perfilado_dbi()` expone los mismos límites y anticipa cuándo el tope de
celdas reducirá la muestra o cuándo hará falta la sonda de bytes.

### El costo se planifica antes de pagarlo

Perfilar una tabla de 158 columnas con el `universo = "tabla_completa"` por
omisión emite 335 consultas, y
327 de ellas escanean, ordenan o agrupan la tabla entera. La cuenta sigue a la
composición y no a la cantidad de columnas: esas mismas 158 columnas, todas de
texto, cuestan 252, porque una mediana pide un orden total por columna numérica.
`muestra` no acota nada de eso —acota lo que se trae a R, no el trabajo del
motor—. Así que el costo se declara y se elige (`benchmark/medir_plan_ancho.R`
reproduce los cuatro números):

```r
plan_perfilado_dbi(
  con, "tabla", universo = "muestra_motor", muestra_motor = 5000
)   # prepara y publica un rango
```

El plan da un **rango** de cuántas consultas va a emitir el perfilado, y lo dice
en `attr(plan, "supuesto")`. No escanea datos para decidir el costo: lee el
esquema, sondea capacidades y consulta el catálogo de la clave primaria. Con
`politica_costo = "por_cardinalidad"` también puede usar una garantía
estructural o una fuente de catálogo. Nunca lanza `COUNT(DISTINCT ...)` sólo
para despejar una duda. Por eso el plan no publica una proyección temporal de
`COUNT(DISTINCT)`: la referencia honesta es el primer lote de distintos medido
durante la ejecución, y el plan no emite consultas de datos.

El extremo inferior es `total`: cuando la fuente de cardinalidad es
desconocida, supone que la política omite la moda. El extremo
superior es `total_maximo` —también publicado como
`total_lotes_rechazados` después de sumar la bisección— y deja abierto el camino
que las ejecuta. Si el motor rechaza lotes, se agregan hasta `2n - 1` sondas por
lote de `n` columnas. El costo real cae entre los dos **cuando el muestreo se puede
construir**: si `universo = "muestra_motor"` y el motor no admite la forma resuelta, el
plan lo declara en `attr(plan, "muestreo")` y excluye del rango las métricas que
dependían de ella. La corrida, por su parte, publica cada una como
`no_disponible` con su motivo: no se informa como medido lo que no se midió.

La procedencia de distintos y la fuente auxiliar de costo son independientes:
`estrategia_distintos` dice cómo se obtiene o se omite `n_distintos`, mientras
`fuente_cardinalidad_costo` dice de dónde sale la proporción que usa
`politica_costo`. La estrategia se anuncia en el plan antes de la corrida y la
política no puede convertir una petición `aproximada_motor` o `catalogo` en un
`COUNT(DISTINCT)` exacto.

Cada lote plano que calcula válidos lleva su propio
`COUNT(*) AS n_total_consulta`, sin una consulta adicional; el plan publica esa
composición.
Cuando hace falta un total del universo separado —por ejemplo, para una muestra
o si no hay agregados planos— sigue siendo una consulta explícita.

Lo que sí es una restricción dura de diseño es que esa predicción **no dependa del
motor**: cada sonda de capacidad gasta un número fijo de consultas aunque acierte
en la primera forma, porque un costo que variara por motor dejaría al usuario
adivinando otra vez.

La moda y la mediana tienen una política de costo explícita. El valor por omisión
es `politica_costo = "todas"` (`"ninguna"` es un alias): el paquete no elige por el usuario. Con
`politica_costo = "por_cardinalidad"`, la corrida resuelve primero las
fuentes estructurales y mide `validos` y `distintos` sólo cuando no hay una
fuente exacta y la estrategia lo permite; después omite por columna sólo la
moda cuando `n_distintos / n_validos >= umbral_cardinalidad`. El umbral por
omisión es `0.5` y se puede mover en cada llamada; gobierna sólo la moda. La
mediana no se omite por cardinalidad: el barrido medido queda plano frente a la
cantidad de valores distintos y su costo lo gobierna el número de filas.
Cada omisión queda en `resumen_tabla$sql` como `omitido_por_costo`, con qué se
omitió, por qué y cómo pedirlo de nuevo: `politica_costo = "todas"` o un umbral
diferente. `meta$decisiones_costo` explica por separado por qué se conserva u
omite cada métrica.
El banco reproducible `benchmark/medir_politica_costo.R` mide el ahorro en
consultas sobre una tabla de 158 columnas.

Pero contar consultas no responde la pregunta que trae quien mira el plan:
catorce consultas sobre dos millones de filas son mucho más trabajo que
doscientas sobre mil. Así que el plan estima además la **magnitud**, en cuentas
de verdad y no en un índice inventado, y la estima en **dos mitades**, porque el
reloj no lo pone siempre el motor. La del motor son `filas_leidas` y
`ordenaciones_completas`, resumidas en `magnitud_motor`; la del cliente son
`columnas_texto` y `pares_texto` —cuántos pares de formas podría comparar en R
el detector de vocabulario sobre la muestra—, resumidas en `magnitud_texto`.
`magnitud` es la mayor de las dos.

En PostgreSQL, cuando la preparacion ya leyo la jerarquia del catalogo, un
`pg_class.reltuples` positivo tambien aporta la magnitud de filas. El plan lo
imprime como estimacion de catalogo, no como medicion, y publica la fuente en
`filas_fuente`/`estimacion_filas`; las proyecciones de trabajo de moda y mediana
quedan en `proyecciones` con el mismo rotulo. Un `reltuples` cero o negativo —el
estado habitual antes de `ANALYZE`— deja desconocidos las filas y esas
proyecciones. Los demas motores conservan el estado desconocido actual.

Contar sólo el motor daba juicios falsos con números ciertos: una tabla del
catálogo de PostGIS de 3.912 filas, con una columna de geometría guardada como
texto, pedía 64.592 lecturas de fila y cero ordenaciones —magnitud `"baja"`— y
tardaba 35 segundos **ya con el presupuesto de trabajo calibrado**, porque lo que
quedaba estaba en comparar formas, que no es una lectura de fila. Es la misma
tabla que arriba tardaba 243 segundos antes de que el presupuesto midiera
trabajo en vez de contar unidades. Al imprimir el plan se ven las dos mitades, y el aviso de
trabajo alto nombra las palancas para acotarlo, que no son las mismas de un lado
que del otro. Es una estimación y lo dice: la del motor cuenta las filas que
habría que leer si ningún índice ayudara, y la del cliente cuenta pares, cuyo
costo unitario depende del largo de los valores —que el plan no conoce sin
leerlos, así que con textos muy largos el tiempo real es varias veces el que
sugiere la referencia—. Los números
publicados no dependen de esos supuestos, así que quien no los comparta puede
rehacer la cuenta.

### La memoria del procesamiento no se estima

`plan_perfilado_dbi()` declara explícitamente que la memoria del procesamiento
**no se estima**: no escala de forma predecible con las filas ni con las celdas,
según lo medido. El plan conserva la **magnitud del trabajo** que se conoce
—filas, celdas y pares de texto—, rotulada como magnitud y no como consumo de
memoria.

Son datos de referencia medidos, **no una predicción para la tabla del plan**
—una corrida única y fechada (2026-08-28) contra un motor de producción remoto
que este repositorio no puede rehacer—: traer la tabla costó aproximadamente 0,13 GB por millón de filas y procesar en R
aproximadamente 1,0-1,5 MB por cada mil filas. La segunda cifra varió por 1,62x
entre tablas de la misma magnitud; esa variación es justamente el motivo por el
que no se usa para estimar.

Ver todas las filas y tener todas las filas en memoria no son lo mismo. En
corridas de referencia, 4,5 millones de filas entraron en 0,6 GB y tardaron 25
segundos en llegar, mientras que procesar 4,5 millones ocupó aproximadamente 7
GB y procesar 12,8 millones aproximadamente 19 GB. El problema observado está
en el procesamiento en R, no en la red ni en el motor.

| dimensión | qué hace |
| --- | --- |
| valores por omisión | todas las métricas exactas sobre la tabla entera |
| `metricas = c("validos", "basicos", "desvio")` | el preset histórico `seguro` |
| `metricas = "validos"` | el preset histórico `conteos` |
| `universo = "muestra_motor"` | una selección del motor materializada en spool cliente; `TABLESAMPLE` u otra fuente limitada se lee una vez y se reutiliza |
| `estrategia_mediana = "aproximada_motor"` | prueba exacto nativo primero y aproximado sólo al final; sólo publica `estimado` si ejecutó una aproximación |

Toda métrica muestreada o aproximada viaja diciéndolo. `estado` distingue
`calculado`, `estimado` y `no_disponible`, y cada fila lleva `universo`,
`tamano_muestra`, `fraccion`, `metodo` y `error_esperado`. En las métricas SQL
muestreadas, `error_esperado` vale `no_estimado` cuando el error podría
calcularse bajo un plan probabilístico pero no se calculó, `no_estimable` para
la moda, la mediana y la cardinalidad observada, que no tienen una cota simple
sin supuestos adicionales o un estimador declarado, y `no_aplica` cuando no
hubo muestreo efectivo. La columna `motivo` explica la distinción; `metodo`,
`tamano_muestra` y `fraccion` conservan las condiciones de la corrida. No se
publica una cota numérica sin una fórmula justificada.

En `resumen_tabla$meta$muestreo`, `tamano_muestra` se conserva por compatibilidad
y registra el tamaño efectivo solicitado a la consulta, `filas_solicitadas`
registra el pedido original y `filas_obtenidas` registra las filas que devolvió
la lectura de `perfil_muestra`. Esta última queda en `NA` si ese bloque no se
solicitó o falló antes de leer. El conteo de distintos tiene su propio estado,
`observado_muestra`: la cardinalidad de una muestra no estima la del universo
sin un estimador declarado, así que se informa por lo que es —lo visto en la
muestra, con el universo al lado—. Un motor sin capacidad de muestreo no rompe:
el muestreo del motor degrada y lo dice en la tabla de cobertura.

Si la consulta de la muestra devuelve cero filas, no hay base para medir las
métricas de alcance `muestra`. Se publican como `NA`, con estado
`no_disponible` y un motivo que nombra la muestra vacía; `n` conserva el conteo
de la tabla completa. Esto no permite concluir que la columna esté vacía, así
que `lupa` no publica cero ni dispara la cascada `sin_valores`.

Una aproximación no se etiqueta como estimada cuando su consulta no se emitió o
no devolvió un valor utilizable. La procedencia de distintos se elige aparte
con `estrategia_distintos`; si se pide `"aproximada_motor"` y el motor no ofrece
la función, queda `no_disponible` y no se ejecuta `COUNT(DISTINCT ...)`.

## 🕳️ El vacío por diseño se declara, no se cuenta como defecto

Todo perfilador asume una forma de tabla. `lupa` asume que una fila es un hecho,
que una columna es un dominio semántico y que una celda vacía debería tener un
valor. La tercera es la que hace daño: una base administrativa está llena de
vacíos legítimos — una vigencia abierta, un salto de patrón en una encuesta,
columnas excluyentes por subtipo, un modelo entidad-atributo-valor. Contarlos
como ausencia es correcto como cuenta y falso como lectura.

`aplicabilidad` declara, por columna, en qué filas la columna corresponde. Las
filas fuera de ese universo salen de `n_faltantes` y `prop_faltantes` en vez de
informarse como ausencia:

```r
perfilar(encuesta, aplicabilidad = list(marca_auto = ~ tiene_auto == "Si"))
```

`columnas_opcionales` cubre el caso más simple, donde la ausencia nunca es
defecto y no hay una regla que escribir. La regla declarada, el universo
resultante y las filas donde la regla no se pudo evaluar quedan en
`cobertura_diagnosticos`: un universo recortado sin constancia sería el mismo
defecto al revés. Las filas cuya regla no se puede determinar se cuentan aparte,
en `n_aplicabilidad_indeterminada`, porque no saber no es lo mismo que no
corresponder.

Declarar el universo habilita además el error simétrico, que antes no tenía
forma de aparecer: `valor_fuera_de_aplicabilidad` informa un valor presente
donde la regla dice que la columna no corresponde.

La misma idea gobierna las pruebas estadísticas. Benford supone un proceso
multiplicativo y los límites de Tukey suponen una distribución; una numeración
—un identificador, un código— no es ninguna de las dos cosas, y que un código
quede lejos de la mediana no dice nada de su calidad.

Reconocer una numeración pide **dos señales, y hacen falta las dos**. La primera
es la **densidad**: un identificador ocupa un tramo compacto de los enteros y una
magnitud se reparte por varios órdenes. La unicidad no sirve, porque un monto
también es casi único. La segunda es la **ausencia de un salto de escala**, y sin
ella la primera hace daño: un valor fuera de escala de hasta el doble del máximo
no baja la densidad lo suficiente, así que un `120` entre edades de 18 a 70 —o un
`2000` detrás de 1..1000— quedaba tapado justo cuando era lo único que había que
ver. Lo que sí los delata es el hueco que abren: 50 y 1.000 donde el típico es 1.

El criterio se eligió midiendo. Un banco de trece columnas con la respuesta
conocida —cinco numeraciones y ocho magnitudes con un dato malo adentro—
comparó cuatro variantes: cruzar las dos señales acierta las trece y **no calla
ningún dato malo real**; la densidad sola acertaba once y callaba dos. Está en
`test-ronda118.R`.

**Y lo que no se corre no se apaga en silencio**: deja su fila en
`cobertura_diagnosticos` con el motivo medido —qué porcentaje de los enteros
cubre la columna, cuántos valores se habrían señalado, cuántas filas de cuántas
trae la muestra—.

La misma idea, al revés, produce un diagnóstico que ninguna señal sola podía
dar. Un `9999` puede ser una edad imposible o un código postal perfectamente
válido, así que la lista de `sentinelas_numericos` no lo trae por omisión y hace
bien: marcarlo siempre rompería cualquier columna donde ese número es un dato.
Pero un valor que **queda fuera de los límites, se repite y tiene forma de dígito
repetido** es un centinela con las tres cosas juntas, y `posible_centinela_numerico`
lo informa sin contarlo como ausencia —eso lo decide quien conoce la columna,
agregándolo a la lista—. Un código postal `9999` repetido treinta veces no es
extremo en su columna; un monto real de `9999` no se repite; un año `1999` no
tiene esa forma.

**La unicidad no se adivina: se pregunta —y en una base, se lee.** Cuando los
datos vienen por DBI, la clave primaria **está declarada en el catálogo del
motor**, así que no se sugiere nada: se lee, en una sola consulta elegida por el
controlador. Y se distingue «esta tabla no declara clave» de «no se pudo
preguntar», que no son lo mismo.

Con un nombre sin calificar, la clave que se publica es la de **la relación que
el motor resuelve**, no la de una homónima de otro esquema: el esquema se le
pregunta al motor con sus mismas reglas. Y por encima de eso, una clave cuyas
columnas no están entre las que se acaban de medir se descarta entera, en
cualquier motor, diciendo por qué. Una clave que no es de la tabla medida es
peor que ninguna.

**La unicidad no se adivina: se pregunta.** Si se declara la clave con
`perfilar(clave = ...)`, que se repita entre las filas con la clave completa es
un hallazgo de severidad `error` con las filas que repiten. Si ninguna fila tiene
la clave completa, el estado es `sin_casos_evaluables` y no `verificada`: cierto
sobre un conjunto vacío es cierto y engañoso a la vez. La advertencia y `meta$clave` separan esa comprobación de
la ausencia de nulos: una clave puede no tener colisiones distintas de los
ausentes y aun así no cumplir `NOT NULL`. Por eso `hallazgos` separa ambos hechos:
`clave_con_ausentes` enumera las filas que impiden esa garantía, y
`clave_no_unica` sólo informa valores repetidos entre filas con la clave completa;
una colisión entre ausentes queda en la primera categoría y no refuta la
unicidad de `meta$clave$unicidad`. Si la trazabilidad agrupa esos ausentes con la
semántica de R, la diferencia queda declarada. Y para no dejar al usuario
ante una casilla en blanco, `sugerir_clave()` ordena las columnas candidatas por
tres señales que publica por separado —si identifica cada fila, si no tiene
ausentes, y cuánto se parece su nombre al de una clave— y `elegir_clave()` las
ofrece numeradas con una opción para escribir otra. Ordenar no es decidir: una
columna única puede ser una clave o una magnitud que no repite, y esa diferencia
no está en los datos.

La lectura DBI también separa catálogo y garantía. Oracle sólo se presenta como
garantizado cuando `STATUS` es `ENABLED` y `VALIDATED`; PostgreSQL y MySQL
publican estados comparables en el catálogo. MariaDB, SQL Server, SQLite y
DuckDB no permiten distinguir aquí ese estado, y por eso una clave visible en
ellos conserva garantía desconocida.

Donde no hay señal que discrimine, `lupa` habla. La cardinalidad alta de una
columna de texto se informa siempre, porque el largo de los valores no distingue
un catálogo de la prosa —falla en los dos sentidos, medido— y el hallazgo no
afirma que sea un defecto: ofrece las tres lecturas posibles para que decida
quien conoce la columna.

`perfilar_por()` responde al formato largo, donde una sola columna apila
dominios sin relación. Perfila cada grupo por separado, descarta las columnas
enteramente ausentes dentro de cada grupo antes de perfilar, y declara lo que
descartó.

`lupa` no infiere el modelo. Pero declarar el universo exige saber que la opción
existe, y quien perfilaba una tabla condicionada sin declarar nada recibía justo
el informe engañoso que la declaración vino a evitar. Así que el paquete **mide
la evidencia y la ofrece**: cuando el valor de una columna decide qué filas
tienen otra, o cuando dos columnas se reparten las filas sin pisarse,
`posible_ausencia_estructural` lo informa con severidad `ok`, la evidencia
medida y la línea exacta que habría que escribir:

```
valor_a  posible_ausencia_estructural  ok
  evidencia   `tipo` predice la presencia de `valor_a` en 100.0 % de 200 filas,
              con 2 valores distintos. La columna corresponde cuando tipo es "A".
  sugerencia  perfilar(datos, aplicabilidad = list(valor_a = ~ tipo == "A"))
```

Sugiere; no decide, y nunca reescribe el universo por su cuenta. Las columnas ya
declaradas quedan fuera del examen. Sobre veinte conjuntos reales que vienen con
R y sesenta tablas al azar con ausencia independiente no produce ninguna señal (corrida rehecha el 2026-08-30, reproducible con `benchmark/banco_ausencia_estructural.R`);
dispara en el modelo entidad-atributo-valor, en el salto de patrón de una
encuesta y en las columnas excluyentes, y se calla cuando el diez por ciento de
las filas rompe la regla, porque entonces la relación existe y no es una regla.

La otra cara es `regla_silencia_ausencia`, también `ok`: una columna declarada
opcional o con universo propio que sigue casi vacía *dentro* de ese universo
recibe un aviso. La declaración funcionó y por eso el perfil salió limpio; el
aviso existe para que eso sea una decisión y no un efecto.

`columnas_personales` cierra el hueco equivalente en la otra declaración que el
paquete no puede hacer solo. Ningún léxico de nombres de columna puede ser
completo: una columna con documentos se puede llamar `cod_benef`, y ninguna
lista de nombres frecuentes la va a reconocer. Lo declarado gana sobre lo
inferido y no se vuelve a examinar.

La viñeta `vacio-por-diseno` documenta el supuesto y las seis formas de tabla
donde no vale.

## 🔢 Unidades declaradas y trazabilidad por fila

Cada hallazgo declara la unidad de `n_evaluados`, `n_afectados` y
`unidad_conteo`. `mayusculas_inconsistentes` y `normalizacion_unicode` usan
`valor_distinto`: cuentan valores distintos, mientras su traza sigue siendo
por fila. Enumera todas las filas que contienen un valor afectado, no sólo las
filas defectuosas. `casi_duplicados_vocabulario` sigue el mismo contrato: su
conteo es la cantidad de valores variantes y su traza enumera todas las filas
cuyo valor pertenece a un grupo seleccionado, incluida la forma dominante.
Por eso una traza puede tener más filas que `n_afectados`: esas filas sirven
para revisar o unificar el grupo completo en el sistema de origen. El detector
de vocabulario es heurístico; la traza es evidencia para revisar, no un
veredicto de que todas esas filas deban corregirse. La traza entrega primero
las formas no dominantes y después las dominantes; la evidencia declara cuántas
filas mostradas pertenecen a cada grupo.

En `patron_raro`, `resumen_patrones` y la evidencia muestran como máximo seis
patrones raros. La trazabilidad usa el conjunto completo de nombres de patrones
raros, sin conservar su tabla de frecuencias, hasta un límite separado de
5.000 nombres. Si se alcanza ese límite, el alcance de la traza es parcial y
`cobertura_diagnosticos` publica el límite; el tope de seis de la presentación
no es por sí mismo una falta de cobertura de la traza. Cada hallazgo publica
también la proporción del patrón dominante y cuántas filas quedaron en patrones
no dominantes excluidos por superar `umbral_patron_raro`. Si ningún patrón
dominante alcanza `umbral_patron_dominante`, no se emite un hallazgo y la no
medición, su proporción observada y la forma de ajustar ese argumento quedan en
`cobertura_diagnosticos`.

`filas_duplicadas` cuenta todas las filas que participan en grupos duplicados,
en línea con la métrica y con la acción predeterminada que las marca. El número
de excedentes queda en la evidencia. `0` significa que se midió que no había
unidades afectadas; `NA` significa que el conteo no se midió. La misma
distinción vale para la cobertura: un diagnóstico que no pudo ejecutarse queda
en `cobertura_diagnosticos`, nunca convertido en cero en silencio.

La traza usa la misma comparación en ambas direcciones que el conteo, incluso
cuando una columna `integer64` no respeta el argumento `fromLast` de
`duplicated()`. Así, `n_afectados` y `trazabilidad$total` siempre describen el
mismo conjunto de filas.

Cuando un hallazgo y su traza no coinciden, `perfilar()` conserva el hallazgo y
emite una advertencia de clase `lupa_trazabilidad_incoherente`. La guarda
compara el total previo al truncado, verifica ambas direcciones y respeta la
unidad declarada; es una red de diagnóstico, no un reemplazo para alinear el
detector y su traza.

## 🛣️ ¿`perfilar()` o `analizar()`?

Usá `perfilar()` cuando quieras el perfil enfocado e inspeccionable: resúmenes
por columna, patrones, tipos inferidos, hallazgos, cobertura de diagnósticos y
relaciones estructurales no declaradas. Usá `analizar()` cuando necesites el
recorrido alrededor de ese perfil: distribuciones, asociaciones, análisis
temporal, clasificación confirmable de variables, propuesta de modelo, plan de
limpieza, cobertura conceptual y tablero.

Si no recibe un modelo ni una propuesta confirmados, `analizar()` mide por
omisión cada fila de la propuesta cuyo estado es `"lista"`. Esa propuesta fue
inferida por `lupa`; nadie la confirmó. `medir_propuesta = FALSE` conserva el
recorrido descriptivo, y también se puede entregar una propuesta o modelo
confirmado. La función agrega de inmediato y conserva el tablero pequeño;
`conservar_detalle_medicion = TRUE` retiene el detalle de medición fila a fila.

**Dónde viven la distribución de valores y las correlaciones.** En `analizar()`,
no en `perfilar()`, y la separación es deliberada: `perfilar()` es la pasada
barata cuyo objeto uno lleva a todos lados, mientras que
`distribucion_valores()` y `detectar_asociaciones()` cuestan más y devuelven
tablas propias. `distribucion_valores()` da frecuencias y cuantiles por columna
con su tope declarado y su marca de truncamiento; `detectar_asociaciones()` da
Pearson entre numéricas —o Spearman, con `metodo_numerico = "spearman"`, para
una relación monótona que no es lineal—, más V de Cramér y eta cuadrado, y cada
fila declara su método y su supuesto. Las dos están exportadas, así que se
pueden llamar sueltas sin pagar el recorrido entero.

## 🚦 Severidades y automatización

`severidad` es un **factor ordenado**: `ok < sospechoso < error`.

- `ok` registra una condición observada aceptable o informativa; no es una
  decisión adversa.
- `sospechoso` es evidencia que merece revisión. Es heurística o necesita
  contexto del dominio y no debe rechazar, reparar ni suprimir datos por sí sola.
- `error` afirma que el control aplicable cruzó su criterio declarado. De los
  tres niveles, es el único candidato para alimentar una barrera automática
  adversa, después de que el proyecto acepte ese criterio y verifique el alcance.

`cobertura_diagnosticos` queda fuera de esa escala. Enumera controles que no se
pudieron evaluar y cómo resolverlos. Una automatización debe revisarla además
de buscar `error`: cero errores no equivale a un perfil limpio si hubo
diagnósticos sin ejecutar. La limpieza siempre es explícita: `aplicar()` sólo
cambia las acciones elegidas de un plan editable.

## ✨ Qué hace lupa en detalle

- Perfila una entrega y muestra faltantes, tipos, patrones, fechas y evidencia
  de datos personales.
- Perfila geometrías `sf` y declara CRS, familias geométricas, vacíos, validez
  planar, dominio de coordenadas y alcance de la caja envolvente; no hace
  análisis espacial.
- Evalúa la ley de Benford sólo cuando se cumplen sus precondiciones y registra
  los casos no aplicables en `cobertura_diagnosticos`.
- Informa `unidades_mixtas` y `monedas_mixtas` en una columna sin convertir
  valores ni suponer tasas de cambio.
- Informa `celdas_multivaluadas` sólo cuando las partes homogéneas coinciden
  con los patrones de la columna.
- Encuentra `relacion_aritmetica_columnas` como identidades o proporciones
  observadas entre columnas numéricas, no como reglas del dominio.
- Encuentra `relacion_orden_columnas` entre columnas comparables y declara el
  alcance de la comparación.
- Encuentra claves, relaciones, dependencias y granularidades de medición que
  nunca fueron declaradas.
- Informa `casi_clave` cuando una columna no temporal tiene al menos 100 filas,
  es casi única y sus colisiones se concentran en pocos valores, que es lo que
  separa una clave con duplicados de un texto libre de alta cardinalidad.
- Permite que cada proyecto defina su marco de calidad, sin imponer un puntaje
  global.
- Mide y evalúa métricas, escalas, reglas de validez y dominios referenciales
  explícitos.
- Produce planes de limpieza editables, aplica sólo acciones elegidas sobre una
  copia y conserva un registro de auditoría.
- Encuentra duplicados aproximados con teselas exactas, MinHash/LSH determinista,
  bloqueo, estimación previa del costo y lotes en disco.
- Repara codificación dañada en R, incluido mojibake repetido y CESU-8, y
  rechaza conversiones con pérdida inseguras.
- Sigue la calidad en el tiempo y crea informes HTML autocontenidos.

## 🧪 Evidencia, con el alcance declarado

Cada comprobación usa una unidad y una referencia declaradas diferentes.
Ninguna estima una exactitud única para todo el paquete.

| Comprobación | Unidad declarada | Resultado | Se reproduce con |
| --- | --- | ---: | --- |
| Pares dirty/clean de Raha | columnas que contienen al menos una celda cambiada | 26/26 recibieron al menos un hallazgo; se señalaron 8 columnas más | `benchmark/medir_lupa.R` |
| Controles limpios construidos | 31 tablas | 0 hallazgos de severidad error; 8 señales para revisar | `test-ronda107.R` |
| Registro real de sanciones | hallazgos de severidad error sobre 2.556 filas | 9/9 confirmados de manera independiente | `benchmark/medir_sanciones.R` |

**Cada fila dice con qué se reproduce, y eso es parte de la comprobación.** Esta
tabla llegó a publicar tres números que nadie podía comprobar desde el
repositorio: uno describía un conjunto de controles que se había reducido de 43
tablas a 31 —y el ruido de 25 señales a 8, o sea que el paquete había mejorado y
el texto seguía diciendo lo viejo—, otro medía nueve defectos plantados cuyo
banco no está acá, y el tercero un registro real sin script que lo bajara. El
primero se volvió a medir, el segundo se sacó hasta que exista su banco, y el
tercero tiene ahora el script.

En los pares de Raha la comparación dirty/clean etiqueta celdas cambiadas; no
etiqueta toda propiedad observable en una columna sin cambios. La revisión
manual encontró una observación apoyada en cada una de las ocho columnas
adicionales: constantes, columnas duplicadas, mayúsculas inconsistentes,
cadenas vacías y texto de alta cardinalidad. Por eso no informamos precisión ni
recall diagnóstico a partir de Raha: 26/26 es cobertura por columna, no evidencia
de que se haya identificado cada celda cambiada. [`benchmark/`](https://github.com/sebollin/lupa/tree/main/benchmark)
reproduce la tabla desde las fuentes publicadas y registra las huellas exactas
de los archivos usados en la corrida publicada, pero sólo cuando `lupa` está
instalado a partir de un build de estas mismas fuentes. Desde la raíz del
repositorio, reproducí esa condición y corré los scripts con:

```sh
R CMD build . && R CMD INSTALL lupa_0.1.0.tar.gz
Rscript benchmark/verdad_raha.R
Rscript benchmark/medir_lupa.R
```

## 🔍 Límites, lugar, estabilidad y referencias

No hay un puntaje de fábrica: `indice_calidad()` devuelve el tablero mientras
el proyecto no declare pesos nombrados completos, y todo índice calculado
conserva cobertura, pesos, transformaciones y universos heterogéneos. El núcleo
es universal y los catálogos son enchufables;
[AGESIC](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/)
v1.6 es una implementación de referencia, no un límite nacional. Tiene dos
dependencias duras, [`cli`](https://cran.r-project.org/package=cli) y
[`data.table`](https://cran.r-project.org/package=data.table), y **no importa
ninguna** a su espacio de nombres: a las dos las llama con `::`. Es deliberado:
`data.table` cambia el significado de `[` en todo paquete que lo importe. Los
paquetes sugeridos habilitan capacidades acotadas:
[`sf`](https://cran.r-project.org/package=sf) habilita el perfilado de
geometrías; [`DBI`](https://cran.r-project.org/package=DBI) aporta la interfaz
de bases y [`RSQLite`](https://cran.r-project.org/package=RSQLite) un backend
para `perfilar_dbi()`; [`stringdist`](https://cran.r-project.org/package=stringdist)
habilita la comparación aproximada de texto.

El trabajo que se puede paralelizar usa **dos hilos por defecto**, que es el tope
que CRAN pide respetar. En su propia máquina puede subirlo por llamada con
`nucleos = 8` o para toda la sesión con `options(lupa.nucleos = 8)`; el resultado
no cambia, sólo cuánto tarda.

### Dónde se ubica

[`skimr`](https://cran.r-project.org/package=skimr) y
[`DataExplorer`](https://cran.r-project.org/package=DataExplorer) exploran;
[`pointblank`](https://cran.r-project.org/package=pointblank),
[`validate`](https://cran.r-project.org/package=validate) y
[`dataquieR`](https://cran.r-project.org/package=dataquieR) expresan o evalúan
reglas; [`zoomerjoin`](https://cran.r-project.org/package=zoomerjoin),
[`textreuse`](https://cran.r-project.org/package=textreuse) y
[`reclin2`](https://cran.r-project.org/package=reclin2) se enfocan en comparar
texto o enlazar registros. [`calidad`](https://github.com/inesscc/calidad),
mantenido por [Klaus Lehmann](https://github.com/Klauslehmann) y
[Ricardo Pizarro](https://github.com/ricardoflopiza), es un eje complementario:
evalúa la calidad de **estimaciones de encuestas**, mientras `lupa` evalúa los
datos tabulares que producen una estimación.

La reparación de codificación sigue en R el enfoque y los datos congelados de
[`ftfy`](https://github.com/rspeer/python-ftfy) 6.3.1, de
[Robyn Speer](https://github.com/rspeer). Incluye once tablas de bytes, CESU-8,
el caso Java `C0 80` y cinco extensiones deliberadas documentadas en
[NEWS](NEWS.md). Reproduce 159 de los 161 casos del corpus distribuido y deja
intactos 30 de los 31 casos negativos. El restante está etiquetado como
*mostly* negative y el corpus mismo espera la reparación: se le arreglan sólo
los controles C1. Deliberadamente no ofrece los pasos de estilo
de `fix_text` de `ftfy`, como deshacer HTML, curvar comillas, normalizar ancho o
normalizar Unicode: cambiar datos legítimos en silencio no es reparar.

### Cita y referencias

~~~r
citation("lupa")
~~~

Las referencias conceptuales son [Batini y Scannapieco
(2016)](https://doi.org/10.1007/978-3-319-24106-7), el [Marco de trabajo de
AGESIC para la Gestión de la Calidad de Datos en Gobierno Digital
v1.6](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/)
y [ISO/IEC 25012:2008](https://www.iso.org/standard/35736.html).

### Estabilidad, contribuciones y licencia

La versión 0.1.0 es anterior a CRAN: la API pública puede cambiar antes de 1.0;
los cambios incompatibles se anunciarán en `NEWS.md` y en las notas de versión,
con una advertencia de deprecación previa siempre que sea viable.

Usá el [issue tracker](https://github.com/sebollin/lupa/issues) para errores,
propuestas y correcciones de documentación. Los contratos estables son las
unidades declaradas, el alcance, la protección y el registro de auditoría; los
detalles de implementación y los tiempos de benchmark pueden cambiar entre
versiones mientras esos contratos sigan siendo ciertos.

`lupa` se distribuye bajo la [GPL-3](https://www.gnu.org/licenses/gpl-3.0.html).
Consultá [`LICENSE.note`](LICENSE.note) por los datos Apache-2.0 derivados de
[`ftfy`](https://github.com/rspeer/python-ftfy) y los datos MIT derivados de
[`naniar`](https://github.com/njtierney/naniar).
