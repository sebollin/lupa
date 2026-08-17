# Perfilar un conjunto de datos

Examina un `data.frame`, `tibble` o `data.table` y devuelve estadísticas
generales, métricas por columna, patrones, formatos de fecha y hallazgos
accionables. Todas las proporciones se expresan en `[0, 1]`.

## Usage

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
  normalizar = TRUE,
  max_filas_hallazgo = 1000L,
  umbral_orden_columnas = 0.95,
  max_columnas_orden = 20L,
  umbral_solapamiento_orden = 0.1,
  umbral_aritmetica = 0.9,
  min_filas_aritmetica = 3L,
  tolerancia_aritmetica = 1e-08,
  max_columnas_aritmetica = 20L,
  casi_duplicados_vocabulario = TRUE,
  max_proporcion_grupo_vocabulario = 0.5,
  umbral_variante_rara_vocabulario = 0.05,
  min_asimetria_vocabulario_corto = 10,
  min_participacion_dominante_vocabulario_corto = 0.5
)
```

## Arguments

- datos:

  Objeto que hereda de `data.frame`.

- nombre:

  Nombre descriptivo del objeto.

- fecha:

  Fecha y hora de la corrida. Se puede fijar para construir series
  reproducibles; se normaliza a UTC.

- muestra:

  Máximo de filas usadas para patrones e inferencia de tipos. Use `Inf`
  para analizar todas las filas.

- max_patrones:

  Máximo de patrones mostrados por columna.

- distinguir_mayusculas:

  Si se distinguen mayúsculas y minúsculas.

- expandir:

  Si se emite un token por carácter en los patrones.

- umbral_alta_cardinalidad:

  Umbral para columnas categóricas.

- umbral_faltantes_sospechoso:

  Umbral inferior de faltantes. El hallazgo se activa al superarlo en
  sentido estricto.

- umbral_faltantes_error:

  Umbral por encima del cual los faltantes son un error; la igualdad
  conserva la severidad sospechosa.

- umbral_patron_raro:

  Máxima frecuencia de un patrón raro.

- umbral_patron_dominante:

  Frecuencia mínima del patrón dominante.

- columnas_sin_ceros:

  Nombres de columnas donde cero no es admisible.

- columnas_no_negativas:

  Nombres de columnas que deben ser no negativas.

- sentinelas_numericos:

  Vector completo de valores numéricos que se interpretan como ausencia.
  [`numeric()`](https://rdrr.io/r/base/numeric.html) los desactiva; las
  cadenas de ausencia se siguen evaluando por separado.

- analizar_dependencias:

  Si se buscan dependencias funcionales entre pares de columnas. Se
  aplica una sola muestra común a toda la tabla.

- umbral_dependencia:

  Cumplimiento mínimo para informar una dependencia.

- umbral_casi_clave_dependencia:

  Tasa de valores distintos a partir de la cual un determinante se
  descarta como casi-clave antes de agrupar.

- max_columnas_dependencias:

  Máximo de columnas que intervienen en la búsqueda, cuyo costo crece
  cuadráticamente.

- datos_personales_permitidos:

  Si la entrega admite datos personales. El valor predeterminado no
  juzga su presencia: la clasificación se informa con severidad `"ok"`.
  Use `FALSE` sólo cuando el contrato de la entrega declare que no deben
  existir.

- proteger_datos_personales:

  Si se reemplazan modas, ejemplos, evidencia y estadísticos de orden
  concretos cuando `poder_discriminante` es medio, alto o verificado.
  Las clasificaciones débiles se conservan como aviso pero no suprimen
  estadísticos. Para conservar todo en el objeto debe desactivarse
  explícitamente;
  [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
  aplica además su propia protección predeterminada.

- validadores_personales:

  Pack o lista nombrada de funciones que reciben un vector de texto y
  devuelven un lógico de igual longitud. `NULL` usa
  [`validadores_uruguay()`](https://sebollin.github.io/lupa/reference/pack_validadores.md)
  por compatibilidad; `FALSE` o
  [`numeric()`](https://rdrr.io/r/base/numeric.html) desactiva la
  verificación de documentos. El nombre del mejor validador queda en el
  fundamento de la clasificación.

- umbral_documento_verificado:

  Proporción mínima de valores que debe aceptar un validador para
  clasificar una forma de documento como `verificado`. Por defecto es
  `0.9`.

- muestra_validadores:

  Máximo de valores usados en el filtro preliminar de cada validador. Si
  la proporción preliminar ya queda bajo el umbral no se valida la
  columna completa; use `Inf` para revisar todos desde el inicio.

- duplicados_aproximados:

  `FALSE` por omisión. Use `TRUE` o una lista de argumentos para
  ejecutar
  [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md)
  y añadir sus pares y hallazgos al perfil. Es un análisis acotado y
  opcional porque no afirma identidad ni debe encarecer todas las
  corridas.

- normalizar:

  Perfil de comparación que se conserva en `meta$normalizacion` y que
  heredan los análisis de duplicados y claves cuando no reciben uno
  explícito. Cambia sólo la representación usada para comparar.

- max_filas_hallazgo:

  Tope de índices de fila que conserva cada trazabilidad disponible. Por
  defecto es `1000`; cuando se supera, el estado queda como `truncada` y
  el total se conserva. Use `Inf` sólo si necesita desactivar
  explícitamente el tope.

- umbral_orden_columnas:

  Cumplimiento mínimo de una relación de orden entre columnas
  comparables. Se usa `0.95` por omisión; con menos de 20 filas
  comparables se permite una sola inversión para no descartar tablas
  pequeñas. El alcance efectivo queda en `meta$orden_columnas`.

- max_columnas_orden:

  Máximo de columnas numéricas o temporales que se comparan entre sí
  para detectar relaciones de orden. Las columnas que exceden el límite
  se conservan en `meta$orden_columnas$columnas_omitidas`.

- umbral_solapamiento_orden:

  Solapamiento mínimo de los rangos intercuartiles para considerar que
  dos columnas representan magnitudes comparables. Por defecto es `0.1`:
  al menos una décima parte del rango intercuartílico más ancho debe ser
  común a ambos. Esto evita interpretar como restricción fila a fila un
  orden explicado sólo por escalas separadas. Si no hay ese
  solapamiento, una brecha con IQR exactamente cero conserva el par
  porque la mitad central sostiene el mismo desplazamiento fila a fila.
  No se aplica una tolerancia oculta. Use `0` para desactivar el filtro
  de magnitud. Ambos criterios se publican en la evidencia. Los pares
  descartados se cuentan en
  `meta$orden_columnas$pares_descartados_magnitud` y los recuperados en
  `meta$orden_columnas$pares_rescatados_brecha_estable`.

- umbral_aritmetica:

  Proporción mínima de filas comparables que deben satisfacer una
  identidad dentro de `tolerancia_aritmetica` para reconocer una
  regularidad aritmética entre columnas numéricas. El valor por omisión
  es `0.9`. Una vez reconocida la relación se informan todas sus
  discrepancias, sin un segundo filtro por su cantidad absoluta. La
  proporción y el criterio efectivos se publican en cada evidencia.

- min_filas_aritmetica:

  Mínimo de filas comparables necesario para evaluar una candidata
  aritmética. Por omisión es `3`.

- tolerancia_aritmetica:

  Tolerancia numérica relativa escalada usada al comparar un valor
  observado y uno esperado. Por omisión es `1e-8`; el criterio completo
  y el valor efectivo se declaran en cada evidencia.

- max_columnas_aritmetica:

  Máximo de columnas numéricas que intervienen en la búsqueda
  aritmética, cuyo costo crece cúbicamente. Por omisión es `20`. Si se
  omiten columnas, `cobertura_diagnosticos` declara el recorte y
  `meta$aritmetica_columnas` conserva los conteos de combinaciones.

- casi_duplicados_vocabulario:

  Lógico que activa el diagnóstico de variantes casi duplicadas dentro
  del vocabulario de cada columna de texto. Por defecto es `TRUE`;
  `FALSE` lo omite sin afectar los demás hallazgos. La distancia es una
  señal heurística, no una prueba de identidad: Jaro–Winkler puede
  agrupar nombres de calles o códigos que sólo comparten un prefijo o un
  sufijo. En vocabularios heterogéneos revise la evidencia como
  sospechosa, declare una regla de dominio o use `FALSE` para desactivar
  este diagnóstico.

- max_proporcion_grupo_vocabulario:

  Proporción máxima del vocabulario que puede abarcar el grupo mayor
  para entregar grupos de variantes. Por defecto es `0.5`; si se supera,
  el alcance declara que el diagnóstico no aplica en vez de entregar un
  bloque que abarque casi toda la columna.

- umbral_variante_rara_vocabulario:

  Proporcion maxima de la columna que puede ocupar una variante breve
  para abrir la comparacion por una edicion.

- min_asimetria_vocabulario_corto:

  Razon minima entre la frecuencia de una forma dominante y una variante
  breve para abrir la comparacion por una edicion.

- min_participacion_dominante_vocabulario_corto:

  Proporcion minima de la columna que debe ocupar la forma dominante en
  la comparacion por una edicion.

## Value

Objeto S3 de clase `perfil`. Cada fila de hallazgos incluye n_evaluados,
n_afectados y unidad_conteo: son conteos de las unidades declaradas (por
ejemplo fila, columna, formato o par). Cuando el camino no puede conocer
un conteo, informa NA, nunca cero. La columna de lista `trazabilidad`
distingue `disponible`, `truncada`, `no_aplica` y `no_disponible`;
cuando corresponde conserva índices de fila acotados por
`max_filas_hallazgo`, el total conocido y el alcance (completo o
parcial). Los índices no contienen valores. Usarlos para extraer filas
de los datos originales puede volver a exponer datos personales; el
paquete no realiza esa extracción y la protección de salidas no
sustituye el control de acceso a los datos de entrada. En la evidencia
de `casi_duplicados_vocabulario`, `clase_diferencia` puede ser
`normalizacion_exacta` (la coincidencia aparece después de normalizar),
`dentro_de_palabra` (la diferencia está dentro de un token),
`token_completo` (cambian tokens completos), `token_unico` (ambos
valores son un solo token y esa distinción estructural no aplica),
`mixta` (el grupo reúne aristas de más de una clase) o `indeterminada`
(no hubo aristas clasificables). Son categorías de evidencia, no
veredictos de identidad. `cobertura_diagnosticos` es una tabla hermana
de `hallazgos`, con una fila por diagnóstico que no pudo evaluarse y las
columnas `diagnostico`, `columna`, `motivo`, `como_resolverlo` y
`dependencia`. Incluye la falta de `stringdist`, `stringi`, `bit64` o
`sf`, y las zonas horarias POSIXt sin declarar. Quien decida
automáticamente sobre un perfil debe revisar
`nrow(perfil$cobertura_diagnosticos)` además de las severidades: un
perfil sin hallazgos y con diagnósticos no evaluados no es un perfil
limpio.

## Details

Los umbrales de faltantes se aplican a la suma de ausentes reales y
faltantes disfrazados y son estrictos: la proporción debe superar el
umbral para generar el nivel correspondiente. La lista de cadenas está
congelada con referencia a
[naniar](https://github.com/njtierney/naniar)::common_na_strings 1.1.0 y
suma extensiones habituales en datos administrativos uruguayos. Las
entradas que [naniar](https://github.com/njtierney/naniar) expresa como
patrones escapados se adaptan a los signos literales de interrogación,
asterisco y punto porque aquí se comparan por igualdad. La lista no
depende de la versión instalada. Los sentinelas numéricos
predeterminados son `-9`, `-99`, `-999`, `-9999` y `999`. La lista es
deliberadamente más corta que
[naniar](https://github.com/njtierney/naniar)::common_na_numbers 1.1.0:
`66`, `77`, `88` y `9999` también pueden ser edades, códigos o años
legítimos. `sentinelas_numericos` representa la política completa, no
una lista que se agrega silenciosamente: use
[`numeric()`](https://rdrr.io/r/base/numeric.html) para desactivar todos
los sentinelas numéricos, o `sentinelas_naniar` para solicitar
explícitamente la lista de naniar.

`muestra` limita sólo el descubrimiento de patrones, la inferencia de
tipos y la detección de formatos de fecha. Las demás métricas y
hallazgos se calculan sobre todas las filas. Por eso
`meta$filas_analizadas` describe el máximo usado por los análisis
muestreados, no el alcance del perfil completo. En cada fila de
`columnas`, `n_filas_analizadas_tipo` y `muestreado_tipo_inferido`
declaran el alcance concreto de `proporcion_tipo_inferido`; no debe
interpretarse esa proporción como si hubiera usado necesariamente toda
la columna.

Una columna cuyo año se expresa con dos dígitos se informa con su
`tipo_inferido` —`"fecha"` o `"fecha-hora"`— pero deja `minimo_fecha`,
`maximo_fecha`, `media_fecha` y `mediana_fecha` en `NA`. No es una
omisión: `23` puede ser 1923 o 2023, y elegir el siglo para calcular un
rango sería inventarlo. El hallazgo `anio_de_dos_digitos` señala esas
columnas, y el rango aparece una vez que el usuario resuelve la
ambigüedad. Una columna de períodos expresados sólo como mes y año
informa la `granularidad` `"mes"` en `formatos_fecha` y deja los
resúmenes de fecha en `NA` con
`estado_resumen_cuantitativo = "granularidad_incompleta"`: asignar el
día 1 para obtener un mínimo o una media también sería inventar un dato.
Si esos períodos son minoritarios dentro de una columna que también
contiene fechas con día, los resúmenes se calculan sólo sobre las fechas
completas y declaran
`estado_resumen_cuantitativo = "calculados_sobre_dias"`, junto con
`n_fechas_resumidas` y `n_fechas_excluidas_granularidad`. El mínimo y el
máximo son entonces condicionales al subconjunto con día; no representan
un rango de toda la columna.

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
separarse en columnas con semántica explícita.

La ley de Benford se evalúa sólo en columnas numéricas con al menos 50
valores finitos, para no agregar cobertura a columnas que ni siquiera
son candidatas. Antes de comparar exige variación, que la columna no
parezca un identificador ni una secuencia correlativa, al menos 100
observaciones positivas utilizables, una proporción de positivos igual a
1 y tres órdenes de magnitud según `log10(max/min)`. Si falla alguna
precondición no emite un hallazgo: la enumera en
`cobertura_diagnosticos`. Si aplica, `meta$benford$resultados` conserva
la distribución observada y esperada por primer dígito, el chi-cuadrado
de Pearson, ocho grados de libertad y el valor p;
`meta$benford$umbrales` publica todos los cortes. Un valor p menor que
`0.01` genera `desviacion_benford` como señal descriptiva para revisar,
no como evidencia de fraude o manipulación. Topes administrativos,
redondeos, precios psicológicos y subsidios de monto fijo son
explicaciones posibles.

Las relaciones aritméticas se buscan sólo entre columnas numéricas
declaradas y con variación: `Date`, `POSIXt`, `difftime`, `integer64`,
texto numérico y columnas constantes no participan. Cada relación
requiere al menos tres filas con valores finitos en todas las columnas
involucradas; los `NA`, `NaN` e infinitos quedan fuera del universo que
publica la evidencia. Para cada terna se prueban las tres orientaciones
de una identidad aditiva; esto cubre sumas y sus restas equivalentes sin
informar tres veces la misma igualdad. En pares proporcionales, `k` es
la mediana de los cocientes finitos cuya base no es cero, pero el
cumplimiento se evalúa después también en las filas con base cero. Si
una identidad aditiva ya relaciona una terna, se omiten las
proporcionalidades redundantes entre su total y sus sumandos; se
conserva la proporcionalidad entre los dos sumandos. Una regularidad
completa se informa con severidad `"ok"`; si alcanza el umbral pero
tiene discrepancias, sigue el criterio de las relaciones de orden y es
`"sospechoso"`. Todo esto describe evidencia observada: no declara una
regla del dominio ni autoriza una corrección. Los valores de texto que
no forman UTF-8 válido tampoco se convierten: se cuentan, se excluyen de
los análisis textuales y generan un hallazgo con sus posiciones. Los
diagnósticos de invisibles incluyen controles C0/C1, espacios Unicode,
marcas direccionales, BOM y otros caracteres de transporte. La evidencia
los muestra como puntos de código; los espacios Unicode se detectan
aunque sólo se normalizan mediante una acción explícita, y ZWJ/ZWNJ se
informan pero se conservan porque pueden ser semánticos. La comparación
de duplicados con `normalizar = TRUE` aplica estas mismas clases sin
borrar ZWJ/ZWNJ.

Los resúmenes de fecha-hora se expresan siempre en UTC y llevan el
sufijo `UTC` en el texto para hacer visible la zona aplicada. El
instante se conserva aunque la columna de entrada use otra zona horaria.
Las columnas `POSIXt` declaran `zona_horaria_origen` y
`n_filas_fecha_civil_distinta_utc`, que cuenta filas cuya fecha civil
cambia al mostrar el instante en UTC. Cuando la fecha civil cambia se
emite un hallazgo `zona_horaria_fecha_hora`; si la zona de origen no
está declarada, el conteo queda en `NA` y `cobertura_diagnosticos`
declara que no se evaluó. La zona se declara en la columna original, por
ejemplo: `attr(x, "tzone") <- "America/Montevideo"`.

El diagnóstico de formas Unicode compara sin modificar el texto y puede
usar el paquete opcional `stringi` para enriquecer la evidencia cuando
existen caracteres no ASCII. `columnas$unicode_evaluado` declara si esa
comprobación pudo ejecutarse; en texto no ASCII sin `stringi` queda
`FALSE`, `n_variantes_unicode` queda en `NA` y `cobertura_diagnosticos`
informa la dependencia ausente. Las columnas ASCII se evalúan siempre y
conservan cero. El perfil de comparación es completamente R base.

Las columnas `sfc` declaran su CRS, los tipos concretos y la dimensión
(`XY`, `XYZ`, `XYM` o `XYZM`), además de geometrías vacías, validez,
dominio y caja envolvente. `POINT`/`MULTIPOINT`,
`LINESTRING`/`MULTILINESTRING` y `POLYGON`/`MULTIPOLYGON` son una misma
familia: el hallazgo de tipos mixtos aparece sólo al combinar familias o
ante `GEOMETRYCOLLECTION`. La validez se calcula siempre con GEOS en el
plano, sin CRS, para que el resultado no dependa de que `s2` esté
instalado; `validez_criterio` publica `"planar"` y `n_validez_evaluados`
publica su universo, que incluye las geometrías vacías porque GEOS sí
devuelve su validez. Si hay una dimensión M, se aplica
[`sf::st_zm()`](https://r-spatial.github.io/sf/reference/st_zm.html) y
se valida la topología XY; `validez_preprocesamiento = "st_zm(x)"`
declara esa transformación. Si el CRS es geográfico, un fallo es
`sospechoso` y no un `error`, porque no afirma invalidez esférica. Las
dimensiones Z y M no se evalúan como medidas: `dimensiones_no_evaluadas`
las enumera y `cobertura_diagnosticos` deja constancia explícita incluso
cuando la validez XY sí pudo calcularse. En un CRS geográfico el dominio
exige longitudes en `[-180, 180]` y latitudes en `[-90, 90]`. Además,
las coordenadas se comparan en longitud/latitud con la `BBOX` del área
de uso incluida en el WKT del CRS. Este control puede detectar valores
en unidades incompatibles —por ejemplo, grados declarados como metros—,
pero no una zona UTM equivocada cuando esa interpretación cae dentro del
área de la zona declarada. Una `BBOX` mundial es un no-op válido; si el
WKT no permite extraer la caja, el dominio queda en `NA` y
`cobertura_diagnosticos` lo declara en lugar de suponer el mundo entero.
Las geometrías vacías se cuentan aparte y no integran el universo del
dominio ni de la bbox. `n_dominio_evaluados` y `n_bbox_evaluados`
publican ambos universos; la bbox se calcula sobre coordenadas crudas de
geometrías no vacías, incluidas las que el dominio marque fuera, y
`bbox_alcance` lo declara. Si todas son vacías, sus conteos evaluados y
fuera de dominio son cero. Sin CRS, `n_fuera_de_dominio` queda en `NA` y
se emite `crs_no_declarado`: nunca se supone EPSG:4326. Si falta el
paquete opcional `sf`, todos esos campos quedan en `NA`, no se emite un
hallazgo geométrico y `cobertura_diagnosticos` registra la dependencia
ausente. El argumento `normalizar` declara el perfil de comparación que
se conserva en `meta$normalizacion`; cambia sólo la representación usada
para comparar, no el texto guardado. `TRUE` usa el perfil
predeterminado, `FALSE` desactiva sus pasos configurables, `"amplio"`
activa los tres pliegues optativos y
[`normalizacion()`](https://sebollin.github.io/lupa/reference/normalizacion.md)
permite declararlos. También admite una lista nombrada por columna.
`meta$normalizacion_fusiones` informa, para cada paso activo, la
diferencia entre los valores distintos con el perfil completo y los
valores distintos con ese paso apagado y todo lo demás igual. Esa
comparación responde cuánto aporta cada paso por separado; sus números
no son aditivos porque dos pasos pueden fundir el mismo par.
`n_distintos` y `n_distintos_normalizados` declaran el total antes y
después del perfil completo. La descomposición canónica es siempre
activa y forma parte de la línea base, no una fila configurable. Con
`normalizar = FALSE` no hay pasos configurables que medir y el informe
de fusiones queda en `NULL`; en un perfil por columna sólo se incluyen
las columnas con algún paso activo. Si
[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md)
recibe un perfil, puede reutilizar este informe ya calculado. El informe
usa el vocabulario completo: `n_distintos` y `n_usados` son el número de
valores distintos realmente comparados y el estado es `exacto`. Las
fusiones son una propiedad de pares, por lo que muestrear valores
aislados podría dejar fuera los dos miembros de cada par y convertir una
fusión real en un cero falso. La normalización se vectoriza para que
este alcance completo no dependa de la cardinalidad de la columna. Las
columnas sin ausentes que tienen al menos 90 % de valores distintos
producen un hallazgo `casi_clave` cuando el valor dominante concentra al
menos la mitad de los duplicados excedentes. La concentración se calcula
sobre las repeticiones posteriores a la primera de cada valor, no sólo
sobre la tasa de distintos. La evidencia declara ambos umbrales, los
valores que colisionan y sus frecuencias. Así una colisión concentrada
queda separada del texto libre de alta cardinalidad con repeticiones
dispersas. Un vector `double` sólo participa si todos sus valores
finitos son enteros; así se conservan identificadores importados con ese
almacenamiento y se excluyen medidas con alguna parte fraccionaria, como
importes o coordenadas. Los vectores `integer64` se tratan como enteros
semánticos. La evidencia del hallazgo declara este criterio y el
recuento observado. Además, si `casi_duplicados_vocabulario = TRUE`, el
perfil busca variantes casi duplicadas en columnas de texto. Agrupa el
vocabulario crudo mediante fusiones exactas de la normalización y
estrellas de distancia centradas en un valor de frecuencia estrictamente
mayor y único; los empates no se fuerzan. No cierra cadenas
transitivamente ni elige una forma canónica. La unidad es el valor
distinto, no la fila, y cada variante conserva su frecuencia. Cada grupo
declara sus distancias mínima y máxima. El límite
`max_proporcion_grupo_vocabulario` evita presentar un grupo que abarque
casi toda la columna como un diagnóstico útil: en ese caso el alcance
dice que el diagnóstico no aplica. Ese límite se activa desde 20 valores
distintos o cuando el grupo mayor ya tiene 10 variantes; sólo suprime el
grupo si además ocupa una fracción mayor que el umbral. Así un grupo de
tres en cuatro valores se entrega, pero quince variantes que ocupan toda
una columna no se presentan como una sola familia. Un grupo grande
dentro de un vocabulario mucho mayor puede seguir pasando si su
proporción es pequeña. El alcance expone ambos cortes. El argumento
permite apagar el detector cuando no corresponde a la tabla. Si hay
pares cercanos pero todas las frecuencias empatan, el alcance declara
que no hubo asimetría para formar una estrella y sugiere
[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md)
para comparar filas. El alcance clasifica cada grupo como
`normalizacion_exacta`, `dentro_de_palabra`, `token_completo`,
`token_unico`, `mixta` o `indeterminada`. La primera indica una
coincidencia tras normalizar; `dentro_de_palabra`, una diferencia dentro
de un token; `token_completo`, diferencias en tokens completos;
`token_unico`, que ambos valores son un único token y la clase
estructural no aplica; `mixta`, aristas de más de una clase; e
`indeterminada`, que no hubo aristas clasificables. Son evidencia
descriptiva, no una decisión sobre identidad. El agrupamiento no cambia
por esa etiqueta. El alcance también descarta aristas de distancia cuyos
números no coinciden. Se comparan las secuencias numéricas, quitando
ceros de relleno y separadores de miles; una diferencia numérica se
trata como otra entidad. Esto puede dejar sin agrupar una errata dentro
de un número, porque no hay evidencia para distinguirla de dos entidades
reales. El alcance declara los valores y pares comparados, los recortes
por cardinalidad y si `stringdist` no estuvo disponible; también informa
cuántas aristas se descartaron por esa regla y el tamaño compatible
después del filtro. El tamaño máximo usado para el límite conserva el
componente potencial antes del filtro numérico, para que una familia de
entidades numeradas no vuelva a presentarse como una sola variante; el
tamaño compatible se muestra aparte. Para mantener acotado el perfil,
por omisión se evalúan hasta 5.000 valores distintos y 2.000.000 de
pares de unidades normalizadas; si se alcanza un límite, la evidencia lo
declara y no presenta el resultado como universo completo. Las fusiones
exactas se informan aun sin ese paquete opcional. Antes de formar esos
grupos se retiran los valores que el mismo perfil ya informó como
`faltantes_disfrazados`. El diagnóstico fuerte de ausencia tiene
precedencia: un centinela no se presenta también como posible variante
de un valor válido. El alcance declara cuántas observaciones retiró este
filtro.

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

- `alto`: una forma muy específica, como un correo, o nombre y forma se
  apoyan mutuamente; se protege;

- `verificado`: al menos tres valores distintos y al menos el 90% cumple
  uno de los validadores personales configurados; se protege incluso sin
  un nombre orientador. El pack uruguayo es el predeterminado, pero
  puede reemplazarse por un
  [`pack_validadores()`](https://sebollin.github.io/lupa/reference/pack_validadores.md)
  de otro país o desactivarse con `FALSE`. La tolerancia del 10% permite
  tipeos aislados sin convertir una columna real en una salida pública;
  el umbral es configurable.

La forma genérica de siete a doce dígitos tiene poder discriminante
débil: también describe importes, teléfonos, facturas e identificadores.
Las formas con separadores sólo se aceptan cuando tienen una estructura
de documento reconocible (por ejemplo, una cédula con grupos y guion o
un RUT con grupos de tres y cuatro dígitos); una fecha ISO, una fecha
con puntos o guiones y separadores arbitrarios no se consideran
documentos. Un validador de dígito que supera el umbral aporta evidencia
verificable y eleva la clasificación; una forma sola nunca se trata como
prueba de identidad.

Este criterio mide capacidad de discriminación, no juzga si la presencia
del dato es correcta. La protección sustituye modas, ejemplos, evidencia
y extremos o medianas que corresponden a observaciones reales. Las
medias y desvíos se conservan como síntesis no ligadas a una fila;
`detalle_proteccion_personal` hace visible la supresión. En fechas de
nacimiento, un hallazgo separado conserva el diagnóstico de valores
anteriores a 1900 o posteriores a la corrida sin publicar las fechas.
Los números escritos como texto reconocen tanto coma como punto decimal
y sus separadores de miles simétricos. Los prefijos de tres letras
separados del número, incluso como sufijo, `U$S` y los símbolos
monetarios se conservan como evidencia; `monedas_mixtas` informa sus
frecuencias sin convertir ni suponer tasas de cambio. Una única moneda o
un símbolo `$` aislado no produce ese hallazgo. Un sufijo de unidad se
reconoce sólo si es `%` o una abreviatura alfabética en minúsculas; por
eso `12 kg` y `13500 g` son unidades, mientras que `12A` y `13B` se
tratan como códigos. Si hay más de una unidad observada,
`unidades_mixtas` informa sus frecuencias y no convierte ni compara sus
magnitudes. Una única unidad no genera ese hallazgo.
`celdas_multivaluadas` es deliberadamente conservador: usa los patrones
de
[`descubrir_patrones()`](https://sebollin.github.io/lupa/reference/descubrir_patrones.md)
y exige partes numéricas, alfanuméricas o identificadoras puntuadas
homogéneas, compatibles con el patrón del resto de la columna. No
interpreta comas en nombres o direcciones como listas; el delimitador,
la cantidad de celdas y la distribución de valores por celda quedan en
la evidencia del hallazgo.

## See also

[`descubrir_patrones()`](https://sebollin.github.io/lupa/reference/descubrir_patrones.md),
[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md),
[`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md),
[`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)

## Examples

``` r
perfil <- perfilar(datos_administrativos)
perfil
#> 
#> ── Perfil de datos: datos_administrativos ──────────────────────────────────────
#> ✖ 5 hallazgos con severidad error
#> ! 14 hallazgos sospechosos
#> ✔ 5 hallazgos informativos ok
#> ℹ 2 diagnosticos no evaluados
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
#>             columna tipo_declarado tipo_inferido proporcion_tipo_inferido
#> 1        id_persona          doble         doble                1.0000000
#> 2            cedula          texto         texto                1.0000000
#> 3  fecha_nacimiento          texto         fecha                0.8461538
#> 4              sexo          texto         texto                1.0000000
#> 5           ingreso          doble         doble                1.0000000
#> 6      departamento          texto         texto                1.0000000
#> 7              pais          texto         texto                1.0000000
#> 8            correo          texto         texto                1.0000000
#> 9          id_copia          doble         doble                1.0000000
#> 10       id_tramite          texto identificador                1.0000000
#>    n_filas_analizadas_tipo muestreado_tipo_inferido  n n_faltantes
#> 1                       13                    FALSE 13           0
#> 2                       13                    FALSE 13           0
#> 3                       13                    FALSE 13           0
#> 4                       12                    FALSE 13           0
#> 5                       13                    FALSE 13           0
#> 6                       13                    FALSE 13           0
#> 7                       13                    FALSE 13           0
#> 8                       13                    FALSE 13           0
#> 9                       13                    FALSE 13           0
#> 10                      13                    FALSE 13           0
#>    prop_faltantes n_faltantes_disfrazados n_faltantes_disfrazados_textuales
#> 1               0                       0                                 0
#> 2               0                       1                                 1
#> 3               0                       1                                 1
#> 4               0                       2                                 2
#> 5               0                       1                                 0
#> 6               0                       0                                 0
#> 7               0                       0                                 0
#> 8               0                       0                                 0
#> 9               0                       0                                 0
#> 10              0                       0                                 0
#>    n_faltantes_disfrazados_numericos prop_faltantes_disfrazados
#> 1                                  0                 0.00000000
#> 2                                  0                 0.07692308
#> 3                                  0                 0.07692308
#> 4                                  0                 0.15384615
#> 5                                  1                 0.07692308
#> 6                                  0                 0.00000000
#> 7                                  0                 0.00000000
#> 8                                  0                 0.00000000
#> 9                                  0                 0.00000000
#> 10                                 0                 0.00000000
#>    n_faltantes_totales prop_faltantes_totales n_distintos tasa_distintos
#> 1                    0             0.00000000          11     0.84615385
#> 2                    1             0.07692308          11     0.84615385
#> 3                    1             0.07692308          12     0.92307692
#> 4                    2             0.15384615           4     0.30769231
#> 5                    1             0.07692308          12     0.92307692
#> 6                    0             0.00000000          11     0.84615385
#> 7                    0             0.00000000           1     0.07692308
#> 8                    0             0.00000000          12     0.92307692
#> 9                    0             0.00000000          11     0.84615385
#> 10                   0             0.00000000          12     0.92307692
#>    secuencia_entera_densa densidad_secuencia_entera
#> 1                   FALSE                        NA
#> 2                   FALSE                        NA
#> 3                   FALSE                        NA
#> 4                   FALSE                        NA
#> 5                   FALSE                        NA
#> 6                   FALSE                        NA
#> 7                   FALSE                        NA
#> 8                   FALSE                        NA
#> 9                   FALSE                        NA
#> 10                  FALSE                        NA
#>    n_posiciones_secuencia_entera n_huecos_secuencia_entera
#> 1                             NA                        NA
#> 2                             NA                        NA
#> 3                             NA                        NA
#> 4                             NA                        NA
#> 5                             NA                        NA
#> 6                             NA                        NA
#> 7                             NA                        NA
#> 8                             NA                        NA
#> 9                             NA                        NA
#> 10                            NA                        NA
#>    umbral_densidad_secuencia_entera min_distintos_secuencia_entera
#> 1                               0.8                             20
#> 2                               0.8                             20
#> 3                               0.8                             20
#> 4                               0.8                             20
#> 5                               0.8                             20
#> 6                               0.8                             20
#> 7                               0.8                             20
#> 8                               0.8                             20
#> 9                               0.8                             20
#> 10                              0.8                             20
#>                 moda frecuencia_moda longitud_minima longitud_maxima
#> 1                  1               2              NA              NA
#> 2  [valor protegido]               2               3              11
#> 3  [valor protegido]               2               4              10
#> 4                  F               6               0               3
#> 5              25000               2              NA              NA
#> 6         Montevideo               2               5              10
#> 7                 UY              13               2               2
#> 8  [valor protegido]               2              10              26
#> 9                  1               2              NA              NA
#> 10             TR001               2               5               5
#>    longitud_media minimo  maximo        media mediana       desvio
#> 1              NA      1      11 5.923077e+00       6 3.546396e+00
#> 2        9.692308     NA      NA           NA      NA           NA
#> 3        9.384615     NA      NA           NA      NA 1.191710e+08
#> 4        1.076923     NA      NA           NA      NA           NA
#> 5              NA    -99 9999999 7.900192e+05   29900 2.767287e+06
#> 6        7.461538     NA      NA           NA      NA           NA
#> 7        2.000000     NA      NA           NA      NA           NA
#> 8       23.923077     NA      NA           NA      NA           NA
#> 9              NA      1      11 5.923077e+00       6 3.546396e+00
#> 10       5.000000     NA      NA           NA      NA           NA
#>    minimo_exacto maximo_exacto      minimo_fecha      maximo_fecha media_fecha
#> 1           <NA>          <NA>              <NA>              <NA>        <NA>
#> 2           <NA>          <NA>              <NA>              <NA>        <NA>
#> 3           <NA>          <NA> [valor protegido] [valor protegido]  1984-12-02
#> 4           <NA>          <NA>              <NA>              <NA>        <NA>
#> 5           <NA>          <NA>              <NA>              <NA>        <NA>
#> 6           <NA>          <NA>              <NA>              <NA>        <NA>
#> 7           <NA>          <NA>              <NA>              <NA>        <NA>
#> 8           <NA>          <NA>              <NA>              <NA>        <NA>
#> 9           <NA>          <NA>              <NA>              <NA>        <NA>
#> 10          <NA>          <NA>              <NA>              <NA>        <NA>
#>        mediana_fecha n_fechas_resumidas n_fechas_excluidas_granularidad n_ceros
#> 1               <NA>                 NA                              NA       0
#> 2               <NA>                 NA                              NA      NA
#> 3  [valor protegido]                 11                               0       0
#> 4               <NA>                 NA                              NA      NA
#> 5               <NA>                 NA                              NA       1
#> 6               <NA>                 NA                              NA      NA
#> 7               <NA>                 NA                              NA      NA
#> 8               <NA>                 NA                              NA      NA
#> 9               <NA>                 NA                              NA       0
#> 10              <NA>                 NA                              NA      NA
#>    n_negativos n_outliers n_nan n_infinito_positivo n_infinito_negativo
#> 1            0          0     0                   0                   0
#> 2           NA         NA     0                   0                   0
#> 3            0          0     0                   0                   0
#> 4           NA         NA     0                   0                   0
#> 5            2          4     0                   0                   0
#> 6           NA         NA     0                   0                   0
#> 7           NA         NA     0                   0                   0
#> 8           NA         NA     0                   0                   0
#> 9            0          0     0                   0                   0
#> 10          NA         NA     0                   0                   0
#>    estado_resumen_cuantitativo zona_horaria_origen
#> 1                   calculados                <NA>
#> 2                    no_aplica                <NA>
#> 3                   calculados                <NA>
#> 4                    no_aplica                <NA>
#> 5                   calculados                <NA>
#> 6                    no_aplica                <NA>
#> 7                    no_aplica                <NA>
#> 8                    no_aplica                <NA>
#> 9                   calculados                <NA>
#> 10                   no_aplica                <NA>
#>    n_filas_fecha_civil_distinta_utc fecha_civil_distinta_utc crs_declarado
#> 1                                NA                       NA          <NA>
#> 2                                NA                       NA          <NA>
#> 3                                NA                       NA          <NA>
#> 4                                NA                       NA          <NA>
#> 5                                NA                       NA          <NA>
#> 6                                NA                       NA          <NA>
#> 7                                NA                       NA          <NA>
#> 8                                NA                       NA          <NA>
#> 9                                NA                       NA          <NA>
#> 10                               NA                       NA          <NA>
#>    tipo_geometria dimension_geometria dimensiones_no_evaluadas
#> 1            <NA>                <NA>                     <NA>
#> 2            <NA>                <NA>                     <NA>
#> 3            <NA>                <NA>                     <NA>
#> 4            <NA>                <NA>                     <NA>
#> 5            <NA>                <NA>                     <NA>
#> 6            <NA>                <NA>                     <NA>
#> 7            <NA>                <NA>                     <NA>
#> 8            <NA>                <NA>                     <NA>
#> 9            <NA>                <NA>                     <NA>
#> 10           <NA>                <NA>                     <NA>
#>    n_geometrias_vacias n_geometrias_invalidas n_validez_evaluados
#> 1                   NA                     NA                  NA
#> 2                   NA                     NA                  NA
#> 3                   NA                     NA                  NA
#> 4                   NA                     NA                  NA
#> 5                   NA                     NA                  NA
#> 6                   NA                     NA                  NA
#> 7                   NA                     NA                  NA
#> 8                   NA                     NA                  NA
#> 9                   NA                     NA                  NA
#> 10                  NA                     NA                  NA
#>    validez_criterio validez_preprocesamiento n_fuera_de_dominio
#> 1              <NA>                     <NA>                 NA
#> 2              <NA>                     <NA>                 NA
#> 3              <NA>                     <NA>                 NA
#> 4              <NA>                     <NA>                 NA
#> 5              <NA>                     <NA>                 NA
#> 6              <NA>                     <NA>                 NA
#> 7              <NA>                     <NA>                 NA
#> 8              <NA>                     <NA>                 NA
#> 9              <NA>                     <NA>                 NA
#> 10             <NA>                     <NA>                 NA
#>    n_dominio_evaluados n_bbox_evaluados bbox_alcance bbox_xmin bbox_xmax
#> 1                   NA               NA         <NA>        NA        NA
#> 2                   NA               NA         <NA>        NA        NA
#> 3                   NA               NA         <NA>        NA        NA
#> 4                   NA               NA         <NA>        NA        NA
#> 5                   NA               NA         <NA>        NA        NA
#> 6                   NA               NA         <NA>        NA        NA
#> 7                   NA               NA         <NA>        NA        NA
#> 8                   NA               NA         <NA>        NA        NA
#> 9                   NA               NA         <NA>        NA        NA
#> 10                  NA               NA         <NA>        NA        NA
#>    bbox_ymin bbox_ymax        detalle_proteccion_personal n_blancos
#> 1         NA        NA                               <NA>         0
#> 2         NA        NA                               <NA>         0
#> 3         NA        NA [estadisticos de orden protegidos]         0
#> 4         NA        NA                               <NA>         1
#> 5         NA        NA                               <NA>         0
#> 6         NA        NA                               <NA>         0
#> 7         NA        NA                               <NA>         0
#> 8         NA        NA                               <NA>         0
#> 9         NA        NA                               <NA>         0
#> 10        NA        NA                               <NA>         0
#>    n_espacios_borde n_variantes_mayusculas n_variantes_unicode unicode_evaluado
#> 1                 0                      0                  NA               NA
#> 2                 0                      0                   0             TRUE
#> 3                 0                      0                   0             TRUE
#> 4                 0                      0                   0             TRUE
#> 5                 0                      0                  NA               NA
#> 6                 0                      0                   0             TRUE
#> 7                 0                      0                   0             TRUE
#> 8                 0                      0                   0             TRUE
#> 9                 0                      0                  NA               NA
#> 10                0                      0                   0             TRUE
#>    n_codificacion_rota n_codificacion_reparable
#> 1                    0                        0
#> 2                    0                        0
#> 3                    0                        0
#> 4                    0                        0
#> 5                    0                        0
#> 6                    0                        0
#> 7                    0                        0
#> 8                    0                        0
#> 9                    0                        0
#> 10                   0                        0
#>    n_codificacion_reparable_parcialmente n_codificacion_irreparable
#> 1                                      0                          0
#> 2                                      0                          0
#> 3                                      0                          0
#> 4                                      0                          0
#> 5                                      0                          0
#> 6                                      0                          0
#> 7                                      0                          0
#> 8                                      0                          0
#> 9                                      0                          0
#> 10                                     0                          0
#>    n_codificacion_no_se_pudo estado_codificacion_reparacion
#> 1                          0                 no_parece_roto
#> 2                          0                 no_parece_roto
#> 3                          0                 no_parece_roto
#> 4                          0                 no_parece_roto
#> 5                          0                 no_parece_roto
#> 6                          0                 no_parece_roto
#> 7                          0                 no_parece_roto
#> 8                          0                 no_parece_roto
#> 9                          0                 no_parece_roto
#> 10                         0                 no_parece_roto
#>    n_codificacion_invalida n_controles_invisibles n_invisibles_eliminables
#> 1                        0                      0                        0
#> 2                        0                      0                        0
#> 3                        0                      0                        0
#> 4                        0                      0                        0
#> 5                        0                      0                        0
#> 6                        0                      0                        0
#> 7                        0                      0                        0
#> 8                        0                      0                        0
#> 9                        0                      0                        0
#> 10                       0                      0                        0
#>    n_espacios_invisibles n_invisibles_significativos n_entidades_html
#> 1                      0                           0                0
#> 2                      0                           0                0
#> 3                      0                           0                0
#> 4                      0                           0                0
#> 5                      0                           0                0
#> 6                      0                           0                0
#> 7                      0                           0                0
#> 8                      0                           0                0
#> 9                      0                           0                0
#> 10                     0                           0                0
#>    n_separadores_en_campo n_numeros_texto proporcion_numeros_texto
#> 1                       0               0                       NA
#> 2                       0               0                       NA
#> 3                       0               0                       NA
#> 4                       0               0                       NA
#> 5                       0               0                       NA
#> 6                       0               0                       NA
#> 7                       0               0                       NA
#> 8                       0               0                       NA
#> 9                       0               0                       NA
#> 10                      0               0                       NA
#>    numero_texto_ambiguo numero_texto_seguro numero_texto_unidad
#> 1                 FALSE               FALSE                    
#> 2                 FALSE               FALSE                    
#> 3                 FALSE               FALSE                    
#> 4                 FALSE               FALSE                    
#> 5                 FALSE               FALSE                    
#> 6                 FALSE               FALSE                    
#> 7                 FALSE               FALSE                    
#> 8                 FALSE               FALSE                    
#> 9                 FALSE               FALSE                    
#> 10                FALSE               FALSE                    
#>    numero_texto_moneda numero_texto_convencion dato_personal_posible
#> 1                                                              FALSE
#> 2                                                               TRUE
#> 3                                                               TRUE
#> 4                                                              FALSE
#> 5                                                              FALSE
#> 6                                                              FALSE
#> 7                                                              FALSE
#> 8                                                               TRUE
#> 9                                                              FALSE
#> 10                                                             FALSE
#>     tipo_dato_personal proporcion_dato_personal
#> 1                 <NA>                       NA
#> 2  documento_identidad                0.8461538
#> 3     fecha_nacimiento                1.0000000
#> 4                 <NA>                       NA
#> 5                 <NA>                       NA
#> 6                 <NA>                       NA
#> 7                 <NA>                       NA
#> 8               correo                0.9230769
#> 9                 <NA>                       NA
#> 10                <NA>                       NA
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
