#' Perfilar un conjunto de datos
#'
#' Examina un `data.frame`, `tibble` o `data.table` y devuelve estadísticas
#' generales, métricas por columna, patrones, formatos de fecha y hallazgos
#' accionables. Todas las proporciones se expresan en `[0, 1]`.
#'
#' Los umbrales de faltantes se aplican a la suma de ausentes reales y
#' faltantes disfrazados y son estrictos: la proporción debe superar el umbral
#' para generar el nivel correspondiente. La lista de cadenas está congelada con referencia a
#' [naniar](https://github.com/njtierney/naniar)::common_na_strings 1.1.0 y suma extensiones habituales en datos
#' administrativos uruguayos. Las entradas que [naniar](https://github.com/njtierney/naniar) expresa como patrones
#' escapados se adaptan a los signos literales de interrogación, asterisco y
#' punto porque aquí se comparan por igualdad. La lista no depende de la
#' versión instalada.
#' Los sentinelas numéricos predeterminados son `-9`, `-99`, `-999`, `-9999` y
#' `999`. La lista es deliberadamente más corta que
#' [naniar](https://github.com/njtierney/naniar)::common_na_numbers 1.1.0:
#' `66`, `77`, `88` y `9999` también pueden
#' ser edades, códigos o años legítimos. `sentinelas_numericos` representa la
#' política completa, no una lista que se agrega silenciosamente: use
#' `numeric()` para desactivar todos los sentinelas numéricos, o
#' `sentinelas_naniar` para solicitar explícitamente la lista de naniar.
#'
#' `muestra` limita sólo el descubrimiento de patrones, la inferencia de tipos y
#' la detección de formatos de fecha. Las demás métricas y hallazgos se calculan
#' sobre todas las filas. Por eso `meta$filas_analizadas` describe el máximo
#' usado por los análisis muestreados, no el alcance del perfil completo.
#' En cada fila de `columnas`, `n_filas_analizadas_tipo` y
#' `muestreado_tipo_inferido` declaran el alcance concreto de
#' `proporcion_tipo_inferido`; no debe interpretarse esa proporción como si
#' hubiera usado necesariamente toda la columna.
#'
#' Una columna cuyo año se expresa con dos dígitos se informa con su
#' `tipo_inferido` —`"fecha"` o `"fecha-hora"`— pero deja `minimo_fecha`,
#' `maximo_fecha`, `media_fecha` y `mediana_fecha` en `NA`. No es una omisión:
#' `23` puede ser 1923 o 2023, y elegir el siglo para calcular un rango sería
#' inventarlo. El hallazgo `anio_de_dos_digitos` señala esas columnas, y el
#' rango aparece una vez que el usuario resuelve la ambigüedad.
#' Una columna de períodos expresados sólo como mes y año informa la
#' `granularidad` `"mes"` en `formatos_fecha` y deja los resúmenes de fecha en
#' `NA` con `estado_resumen_cuantitativo = "granularidad_incompleta"`: asignar
#' el día 1 para obtener un mínimo o una media también sería inventar un dato.
#' Si esos períodos son minoritarios dentro de una columna que también contiene
#' fechas con día, los resúmenes se calculan sólo sobre las fechas completas y
#' declaran `estado_resumen_cuantitativo = "calculados_sobre_dias"`, junto con
#' `n_fechas_resumidas` y `n_fechas_excluidas_granularidad`. El mínimo y el máximo
#' son entonces condicionales al subconjunto con día; no representan un rango
#' de toda la columna.
#'
#' Para números ordinarios, los estadísticos cuantitativos se calculan sólo con
#' valores finitos; `n_nan`, `n_infinito_positivo` y `n_infinito_negativo`
#' declaran lo excluido. En columnas `integer64` que exceden el entero máximo
#' representable exactamente por `double`, `minimo` y `maximo` quedan en `NA` y
#' los extremos exactos se conservan en `minimo_exacto` y `maximo_exacto`.
#' Una columna de listas intenta contar sus valores distintos; si la clase no
#' admite comparación, informa `NA` en lugar de afirmar cero.
#' Las columnas matriciales se conservan como una unidad por fila: `n` informa
#' las filas de la tabla, pero los estadísticos por valor quedan en `NA` y un
#' hallazgo explica que deben separarse en columnas con semántica explícita.
#'
#' La ley de Benford se evalúa sólo en columnas numéricas con al menos 50
#' valores finitos, para no agregar cobertura a columnas que ni siquiera son
#' candidatas. Antes de comparar exige variación, que la columna no parezca un
#' identificador ni una secuencia correlativa, al menos 100 observaciones
#' positivas utilizables, una proporción de positivos igual a 1 y tres órdenes
#' de magnitud según `log10(max/min)`. Si falla alguna precondición no emite un
#' hallazgo: la enumera en `cobertura_diagnosticos`. Si aplica,
#' `meta$benford$resultados` conserva la distribución observada y esperada por
#' primer dígito, el chi-cuadrado de Pearson, ocho grados de libertad y el valor
#' p; `meta$benford$umbrales` publica todos los cortes. Un valor p menor que
#' `0.01` genera `desviacion_benford` como señal descriptiva para revisar, no
#' como evidencia de fraude o manipulación. Topes administrativos, redondeos,
#' precios psicológicos y subsidios de monto fijo son explicaciones posibles.
#'
#' Las relaciones aritméticas se buscan sólo entre columnas numéricas
#' declaradas y con variación: `Date`, `POSIXt`, `difftime`, `integer64`, texto
#' numérico y columnas constantes no participan. Cada relación requiere al
#' menos tres filas con valores finitos en todas las columnas involucradas;
#' los `NA`, `NaN` e infinitos quedan fuera del universo que publica la
#' evidencia. Para cada terna se prueban las tres orientaciones de una identidad
#' aditiva; esto cubre sumas y sus restas equivalentes sin informar tres veces
#' la misma igualdad. En pares proporcionales, `k` es la mediana de los cocientes
#' finitos cuya base no es cero, pero el cumplimiento se evalúa después también
#' en las filas con base cero. Si una identidad aditiva ya relaciona una terna,
#' se omiten las proporcionalidades redundantes entre su total y sus sumandos;
#' se conserva la proporcionalidad entre los dos sumandos. Una regularidad
#' completa se informa con severidad `"ok"`; si alcanza el umbral pero tiene
#' discrepancias, sigue el criterio de las relaciones de orden y es
#' `"sospechoso"`. Todo esto describe evidencia observada: no declara una regla
#' del dominio ni autoriza una corrección.
#' Los valores de texto que no forman UTF-8 válido tampoco se convierten: se
#' cuentan, se excluyen de los análisis textuales y generan un hallazgo con sus
#' posiciones. Los diagnósticos de invisibles incluyen controles C0/C1,
#' espacios Unicode, marcas direccionales, BOM y otros caracteres de transporte.
#' La evidencia los muestra como puntos de código; los espacios Unicode se
#' detectan aunque sólo se normalizan mediante una acción explícita, y ZWJ/ZWNJ
#' se informan pero se conservan porque pueden ser semánticos. La comparación
#' de duplicados con `normalizar = TRUE` aplica estas mismas clases sin borrar
#' ZWJ/ZWNJ.
#'
#' Los resúmenes de fecha-hora se expresan siempre en UTC y llevan el sufijo
#' `UTC` en el texto para hacer visible la zona aplicada. El instante se
#' conserva aunque la columna de entrada use otra zona horaria. Las columnas
#' `POSIXt` declaran `zona_horaria_origen` y
#' `n_filas_fecha_civil_distinta_utc`, que cuenta filas cuya fecha civil cambia
#' al mostrar el instante en UTC. Cuando la fecha civil cambia se emite un
#' hallazgo `zona_horaria_fecha_hora`; si la zona de origen no está declarada,
#' el conteo queda en `NA` y `cobertura_diagnosticos` declara que no se evaluó.
#' La zona se declara en la columna original, por ejemplo:
#' `attr(x, "tzone") <- "America/Montevideo"`.
#'
#' El diagnóstico de formas Unicode compara sin modificar el texto y puede usar
#' el paquete opcional `stringi` para enriquecer la evidencia cuando existen
#' caracteres no ASCII. `columnas$unicode_evaluado` declara si esa comprobación
#' pudo ejecutarse; en texto no ASCII sin `stringi` queda `FALSE`,
#' `n_variantes_unicode` queda en `NA` y `cobertura_diagnosticos` informa la
#' dependencia ausente. Las columnas ASCII se evalúan siempre y conservan cero.
#' El perfil de comparación es completamente R base.
#'
#' Las columnas `sfc` declaran su CRS, los tipos concretos y la dimensión
#' (`XY`, `XYZ`, `XYM` o `XYZM`), además de geometrías vacías, validez, dominio y
#' caja envolvente. `POINT`/`MULTIPOINT`, `LINESTRING`/`MULTILINESTRING` y
#' `POLYGON`/`MULTIPOLYGON` son una misma familia: el hallazgo de tipos mixtos
#' aparece sólo al combinar familias o ante `GEOMETRYCOLLECTION`.
#' La validez se calcula siempre con GEOS en el plano, sin CRS, para que el
#' resultado no dependa de que `s2` esté instalado; `validez_criterio` publica
#' `"planar"` y `n_validez_evaluados` publica su universo, que incluye las
#' geometrías vacías porque GEOS sí devuelve su validez. Si hay una dimensión
#' M, se aplica `sf::st_zm()` y se valida la topología XY;
#' `validez_preprocesamiento = "st_zm(x)"` declara esa transformación. Si el
#' CRS es geográfico, un fallo es `sospechoso` y no un `error`, porque no afirma
#' invalidez esférica. Las dimensiones Z y M no se evalúan como medidas:
#' `dimensiones_no_evaluadas` las enumera y `cobertura_diagnosticos` deja
#' constancia explícita incluso cuando la validez XY sí pudo calcularse.
#' En un CRS geográfico el dominio exige longitudes en `[-180, 180]` y latitudes
#' en `[-90, 90]`. Además, las coordenadas se comparan en longitud/latitud con
#' la `BBOX` del área de uso incluida en el WKT del CRS. Este control puede
#' detectar valores en unidades incompatibles —por ejemplo, grados declarados
#' como metros—, pero no una zona UTM equivocada cuando esa interpretación cae
#' dentro del área de la zona declarada. Una `BBOX` mundial es un no-op válido;
#' si el WKT no permite extraer la caja, el dominio queda en `NA` y
#' `cobertura_diagnosticos` lo declara en lugar de suponer el mundo entero.
#' Las geometrías vacías se cuentan aparte y no integran el universo del dominio
#' ni de la bbox. `n_dominio_evaluados` y `n_bbox_evaluados` publican ambos
#' universos; la bbox se calcula sobre coordenadas crudas de geometrías no
#' vacías, incluidas las que el dominio marque fuera, y `bbox_alcance` lo
#' declara. Si todas son vacías, sus conteos evaluados y fuera de dominio son
#' cero. Sin CRS,
#' `n_fuera_de_dominio` queda en `NA` y se emite `crs_no_declarado`: nunca se
#' supone EPSG:4326. Si falta el paquete opcional `sf`, todos esos campos quedan
#' en `NA`, no se emite un hallazgo geométrico y `cobertura_diagnosticos`
#' registra la dependencia ausente.
#' El argumento `normalizar` declara el perfil de comparación que se conserva
#' en `meta$normalizacion`; cambia sólo la representación usada para comparar,
#' no el texto guardado. `TRUE` usa el perfil predeterminado, `FALSE` desactiva
#' sus pasos configurables, `"amplio"` activa los tres pliegues optativos y
#' [normalizacion()] permite declararlos. También admite una lista nombrada por
#' columna. `meta$normalizacion_fusiones` informa, para cada paso activo, la
#' diferencia entre los valores distintos con el perfil completo y los valores
#' distintos con ese paso apagado y todo lo demás igual. Esa comparación
#' responde cuánto aporta cada paso por separado; sus números no son aditivos
#' porque dos pasos pueden fundir el mismo par. `n_distintos` y
#' `n_distintos_normalizados` declaran el total antes y después del perfil
#' completo. La descomposición canónica es siempre activa y forma parte de la
#' línea base, no una fila configurable.
#' Con `normalizar = FALSE` no hay pasos configurables que medir y el informe
#' de fusiones queda en `NULL`; en un perfil por columna sólo se incluyen las
#' columnas con algún paso activo. Si `detectar_duplicados_aproximados()` recibe
#' un perfil, puede reutilizar este informe ya calculado.
#' El informe usa el vocabulario completo: `n_distintos` y `n_usados` son el
#' número de valores distintos realmente comparados y el estado es `exacto`.
#' Las fusiones son una propiedad de pares, por lo que muestrear valores
#' aislados podría dejar fuera los dos miembros de cada par y convertir una
#' fusión real en un cero falso. La normalización se vectoriza para que este
#' alcance completo no dependa de la cardinalidad de la columna.
#' Las columnas sin ausentes que tienen al menos 100 filas y 90 % de valores
#' distintos producen un hallazgo `casi_clave` cuando el valor dominante
#' concentra al menos la mitad de los duplicados excedentes. La concentración
#' se calcula sobre las repeticiones posteriores a la primera de cada valor, no
#' sólo sobre la tasa de distintos. La evidencia declara el mínimo de filas,
#' ambos umbrales, los valores que colisionan y sus frecuencias. Las variables
#' con rol propuesto `fecha`, incluidas fecha-hora, se excluyen. Así una colisión
#' concentrada queda separada del texto libre de alta cardinalidad con
#' repeticiones dispersas.
#' Un vector `double` sólo participa si todos sus valores finitos son enteros;
#' así se conservan identificadores importados con ese almacenamiento y se
#' excluyen medidas con alguna parte fraccionaria, como importes o coordenadas.
#' Los vectores `integer64` se tratan como enteros semánticos. La evidencia del
#' hallazgo declara este criterio y el recuento observado.
#' Además, si `casi_duplicados_vocabulario = TRUE`, el perfil busca variantes
#' casi duplicadas en columnas de texto. Agrupa el vocabulario crudo mediante
#' fusiones exactas de la normalización y estrellas de distancia centradas en
#' un valor de frecuencia estrictamente mayor y único; los empates no se fuerzan.
#' No cierra cadenas transitivamente ni elige una forma canónica. La unidad es
#' el valor distinto, no la fila, y cada variante
#' conserva su frecuencia. Cada grupo declara sus distancias mínima y máxima.
#' El límite `max_proporcion_grupo_vocabulario` evita presentar un grupo que
#' abarque casi toda la columna como un diagnóstico útil: en ese caso el alcance
#' dice que el diagnóstico no aplica. Ese límite se activa desde 20 valores
#' distintos o cuando el grupo mayor ya tiene 10 variantes; sólo suprime el
#' grupo si además ocupa una fracción mayor que el umbral. Así un grupo de tres
#' en cuatro valores se entrega, pero quince variantes que ocupan toda una
#' columna no se presentan como una sola familia. Un grupo grande dentro de un
#' vocabulario mucho mayor puede seguir pasando si su proporción es pequeña. El
#' alcance expone ambos cortes. El argumento permite apagar el detector cuando
#' no corresponde a la tabla. Si hay pares cercanos pero todas las frecuencias
#' empatan, el alcance declara que no hubo asimetría para formar una estrella y
#' sugiere [detectar_duplicados_aproximados()] para comparar filas. El alcance
#' clasifica cada grupo como `normalizacion_exacta`, `dentro_de_palabra`,
#' `token_completo`, `token_unico`, `mixta` o `indeterminada`. La primera indica
#' una coincidencia tras normalizar; `dentro_de_palabra`, una diferencia dentro
#' de un token; `token_completo`, diferencias en tokens completos;
#' `token_unico`, que ambos valores son un único token y la clase estructural no
#' aplica; `mixta`, aristas de más de una clase; e `indeterminada`, que no hubo
#' aristas clasificables. Son evidencia descriptiva, no una decisión sobre
#' identidad. El agrupamiento no cambia por esa etiqueta. El alcance
#' también descarta aristas de distancia cuyos números no coinciden. Se comparan
#' las secuencias numéricas, quitando ceros de relleno y separadores de miles;
#' una diferencia numérica se trata como otra entidad. Esto puede dejar sin
#' agrupar una errata dentro de un número, porque no hay evidencia para
#' distinguirla de dos entidades reales. El alcance declara los valores y pares
#' comparados, los recortes por cardinalidad y si `stringdist` no estuvo
#' disponible; también informa cuántas aristas se descartaron por esa regla y
#' el tamaño compatible después del filtro. El tamaño máximo usado para el
#' límite conserva el componente potencial antes del filtro numérico, para que
#' una familia de entidades numeradas no vuelva a presentarse como una sola
#' variante; el tamaño compatible se muestra aparte. Para mantener acotado el
#' perfil, por omisión se evalúan hasta
#' 5.000 valores distintos y 2.000.000 de pares de unidades normalizadas; si
#' se alcanza un límite, la evidencia lo declara y no presenta el resultado
#' como universo completo. Las fusiones exactas se informan aun sin ese paquete
#' opcional.
#' Antes de formar esos grupos se retiran los valores que el mismo perfil ya
#' informó como `faltantes_disfrazados`. El diagnóstico fuerte de ausencia tiene
#' precedencia: un centinela no se presenta también como posible variante de un
#' valor válido. El alcance declara cuántas observaciones retiró este filtro.
#'
#' La clasificación de posibles datos personales es más amplia que la
#' protección. Cada clasificación declara `poder_discriminante` y `proteger`:
#'
#' - `debil`: una forma genérica, como siete a doce dígitos, coincide también
#'   con importes, facturas y códigos; se informa pero no se ocultan valores;
#' - `medio`: el nombre de la columna expresa una categoría personal (por
#'   ejemplo `telefono` o `fecha_nacimiento`); se protege aunque sus valores no
#'   se puedan validar. El nombre tiene prioridad sobre una forma numérica
#'   genérica y también determina la etiqueta de tipo;
#' - `alto`: una forma muy específica, como un correo, o nombre y forma se
#'   apoyan mutuamente; se protege;
#' - `verificado`: al menos tres valores distintos y al menos el 90% cumple uno
#'   de los validadores personales configurados; se protege incluso sin un
#'   nombre orientador. El pack uruguayo es el predeterminado, pero puede
#'   reemplazarse por un `pack_validadores()` de otro país o desactivarse con
#'   `FALSE`. La tolerancia del 10% permite tipeos aislados sin convertir una
#'   columna real en una salida pública; el umbral es configurable.
#'
#' La forma genérica de siete a doce dígitos tiene poder discriminante débil:
#' también describe importes, teléfonos, facturas e identificadores. Las formas
#' con separadores sólo se aceptan cuando tienen una estructura de documento
#' reconocible (por ejemplo, una cédula con grupos y guion o un RUT con grupos
#' de tres y cuatro dígitos); una fecha ISO, una fecha con puntos o guiones y
#' separadores arbitrarios no se consideran documentos. Un validador de dígito
#' que supera el umbral aporta evidencia verificable y eleva la clasificación;
#' una forma sola nunca se trata como prueba de identidad.
#'
#' Este criterio mide capacidad de discriminación, no juzga si la presencia del
#' dato es correcta. La protección sustituye modas, ejemplos, evidencia y
#' extremos o medianas que corresponden a observaciones reales. Las medias y
#' desvíos se conservan como síntesis no ligadas a una fila;
#' `detalle_proteccion_personal` hace visible la supresión. En fechas de
#' nacimiento, un hallazgo separado conserva el diagnóstico de valores
#' anteriores a 1900 o posteriores a la corrida sin publicar las fechas.
#' Los números escritos como texto reconocen tanto coma como punto decimal y
#' sus separadores de miles simétricos. Los prefijos de tres letras separados
#' del número, incluso como sufijo, `U$S` y los símbolos monetarios se
#' conservan como evidencia; `monedas_mixtas` informa sus frecuencias sin
#' convertir ni suponer tasas de cambio. Una única moneda o un símbolo `$`
#' aislado no produce ese hallazgo.
#' Un sufijo de unidad se reconoce sólo si es `%` o una abreviatura alfabética
#' en minúsculas; por eso `12 kg` y `13500 g` son unidades, mientras que
#' `12A` y `13B` se tratan como códigos. Si hay más de una unidad observada,
#' `unidades_mixtas` informa sus frecuencias y no convierte ni compara sus
#' magnitudes. Una única unidad no genera ese hallazgo.
#' `celdas_multivaluadas` es deliberadamente conservador: usa los patrones de
#' [descubrir_patrones()] y exige partes numéricas, alfanuméricas o
#' identificadoras puntuadas homogéneas, compatibles con el patrón del resto de
#' la columna. No interpreta comas en nombres o direcciones como listas; el
#' delimitador, la cantidad de celdas y la distribución de valores por celda
#' quedan en la evidencia del hallazgo.
#'
#' @param datos Objeto que hereda de `data.frame`.
#' @param nombre Nombre descriptivo del objeto.
#' @param fecha Fecha y hora de la corrida. Se puede fijar para construir series
#'   reproducibles; se normaliza a UTC.
#' @param muestra Máximo de filas usadas para patrones e inferencia de tipos.
#'   Use `Inf` para analizar todas las filas.
#' @param max_patrones Máximo de patrones mostrados por columna.
#' @param distinguir_mayusculas Si se distinguen mayúsculas y minúsculas.
#' @param expandir Si se emite un token por carácter en los patrones.
#' @param clave Nombres de las columnas que identifican una fila. Cuando se
#'   declaran, la trazabilidad de cada hallazgo trae además el valor de esas
#'   columnas para las filas señaladas, de modo que el caso se pueda verificar
#'   en el sistema de origen sin abrir la tabla. El índice de fila se conserva
#'   siempre. La clave declarada se trata como sensible: si la protección de
#'   datos personales está activa y alguna de esas columnas se clasifica como
#'   personal, sus valores salen enmascarados igual que la evidencia.
#' @param umbral_alta_cardinalidad Umbral sobre la tasa de valores distintos
#'   de una columna categórica. No alcanza por sí solo: el hallazgo exige
#'   además al menos diez valores distintos, porque con pocos la tasa está
#'   dominada por el tamaño de la tabla —dos valores en tres filas dan 0,67 y
#'   superan cualquier umbral razonable— y una columna de dos valores no
#'   puede tener cardinalidad alta.
#' @param umbral_faltantes_sospechoso Umbral inferior de faltantes. El
#'   hallazgo se activa al superarlo en sentido estricto.
#' @param umbral_faltantes_error Umbral por encima del cual los faltantes son
#'   un error; la igualdad conserva la severidad sospechosa.
#' @param umbral_patron_raro Máxima frecuencia de un patrón raro.
#' @param umbral_patron_dominante Frecuencia mínima del patrón dominante.
#' @param columnas_opcionales Nombres de columnas donde la ausencia no es un
#'   defecto. Su universo de completitud son las celdas presentes, y
#'   `cobertura_diagnosticos` declara el recorte. Sirve para el vacío por
#'   diseño: un historial con vigencia abierta, una columna que sólo
#'   corresponde a algunas filas.
#' @param aplicabilidad Lista con nombre por columna, donde cada elemento es
#'   una fórmula de un solo lado evaluada sobre `datos` — por ejemplo
#'   `list(marca_auto = ~ tiene_auto == "Si")`. Las filas donde el predicado no
#'   se cumple salen del universo de esa columna: no cuentan como ausencia. Las
#'   filas donde el predicado no se puede determinar se declaran aparte, sin
#'   contarse ni como aplicables ni como no aplicables. Un valor presente fuera
#'   del universo produce el hallazgo `valor_fuera_de_aplicabilidad`, que es el
#'   error simétrico y hoy no tiene otra forma de aparecer.
#'
#'   `lupa` no infiere el universo: si nadie lo declara, toda la columna aplica
#'   y el resultado es el de siempre. Declararlo es lo que distingue el vacío
#'   por diseño del vacío por error, y sin esa distinción una tabla sana puede
#'   informar completitud baja siendo completa.
#' @param columnas_sin_ceros Nombres de columnas donde cero no es admisible.
#' @param columnas_no_negativas Nombres de columnas que deben ser no negativas.
#' @param sentinelas_numericos Vector completo de valores numéricos que se
#'   interpretan como ausencia. `numeric()` los desactiva; las cadenas de
#'   ausencia se siguen evaluando por separado.
#' @param analizar_dependencias Si se buscan dependencias funcionales entre
#'   pares de columnas. Se aplica una sola muestra común a toda la tabla.
#' @param umbral_dependencia Cumplimiento mínimo para informar una dependencia.
#' @param umbral_casi_clave_dependencia Tasa de valores distintos a partir de
#'   la cual un determinante se descarta como casi-clave antes de agrupar.
#' @param max_columnas_dependencias Máximo de columnas que intervienen en la
#'   búsqueda, cuyo costo crece cuadráticamente.
#' @param datos_personales_permitidos Si la entrega admite datos personales.
#'   El valor predeterminado no juzga su presencia: la clasificación se informa
#'   con severidad `"ok"`. Use `FALSE` sólo cuando el contrato de la entrega
#'   declare que no deben existir.
#' @param proteger_datos_personales Si se reemplazan modas, ejemplos, evidencia
#'   y estadísticos de orden concretos cuando `poder_discriminante` es medio,
#'   alto o verificado. Las clasificaciones débiles se conservan como aviso pero
#'   no suprimen estadísticos. Para conservar todo en el objeto debe desactivarse
#'   explícitamente; [reportar()] aplica además su propia protección
#'   predeterminada.
#' @param validadores_personales Pack o lista nombrada de funciones que reciben
#'   un vector de texto y devuelven un lógico de igual longitud. `NULL` usa
#'   `validadores_uruguay()` por compatibilidad; `FALSE` o `numeric()` desactiva
#'   la verificación de documentos. El nombre del mejor validador queda en el
#'   fundamento de la clasificación.
#' @param umbral_documento_verificado Proporción mínima de valores que debe
#'   aceptar un validador para clasificar una forma de documento como
#'   `verificado`. Por defecto es `0.9`.
#' @param muestra_validadores Máximo de valores usados en el filtro preliminar
#'   de cada validador. Si la proporción preliminar ya queda bajo el umbral no
#'   se valida la columna completa; use `Inf` para revisar todos desde el inicio.
#' @param duplicados_aproximados `FALSE` por omisión. Use `TRUE` o una lista de
#'   argumentos para ejecutar [detectar_duplicados_aproximados()] y añadir sus
#'   pares y hallazgos al perfil. Es un análisis acotado y opcional porque no
#'   afirma identidad ni debe encarecer todas las corridas.
#' @param normalizar Perfil de comparación que se conserva en `meta$normalizacion`
#'   y que heredan los análisis de duplicados y claves cuando no reciben uno
#'   explícito. Cambia sólo la representación usada para comparar.
#' @param casi_duplicados_vocabulario Lógico que activa el diagnóstico de
#'   variantes casi duplicadas dentro del vocabulario de cada columna de texto.
#'   Por defecto es `TRUE`; `FALSE` lo omite sin afectar los demás hallazgos.
#'   La distancia es una señal heurística, no una prueba de identidad: Jaro--Winkler
#'   puede agrupar nombres de calles o códigos que sólo comparten un prefijo o un
#'   sufijo. En vocabularios heterogéneos revise la evidencia como sospechosa,
#'   declare una regla de dominio o use `FALSE` para desactivar este diagnóstico.
#' @param max_proporcion_grupo_vocabulario Proporción máxima del vocabulario
#'   que puede abarcar el grupo mayor para entregar grupos de variantes. Por
#'   defecto es `0.5`; si se supera, el alcance declara que el diagnóstico no
#'   aplica en vez de entregar un bloque que abarque casi toda la columna.
#' @param umbral_variante_rara_vocabulario Proporcion maxima de la columna que
#'   puede ocupar una variante breve para abrir la comparacion por una edicion.
#' @param min_asimetria_vocabulario Razón mínima entre la frecuencia de la
#'   forma dominante y la de la variante para abrir un grupo por la vía general
#'   de distancia. Por omisión `2`. Una asimetría de `1,5` significa que las dos
#'   formas son casi igual de comunes, que es evidencia muy floja de una errata:
#'   medido sobre tablas limpias y sobre erratas sembradas, los falsos positivos
#'   están entre `1,0` y `1,5` y las erratas reales desde `9,0`.
#' @param min_asimetria_vocabulario_corto Razon minima entre la frecuencia de
#'   una forma dominante y una variante breve para abrir la comparacion por una
#'   edicion.
#' @param min_participacion_dominante_vocabulario_corto Proporcion minima de la
#'   columna que debe ocupar la forma dominante en la comparacion por una
#'   edicion.
#' @param max_filas_hallazgo Tope de índices de fila que conserva cada
#'   trazabilidad disponible. Por defecto es `1000`; cuando se supera, el
#'   estado queda como `truncada` y el total se conserva. Use `Inf` sólo si
#'   necesita desactivar explícitamente el tope.
#' @param umbral_orden_columnas Cumplimiento mínimo de una relación de orden
#'   entre columnas comparables. Se usa `0.95` por omisión; con menos de 20
#'   filas comparables se permite una sola inversión para no descartar tablas
#'   pequeñas. El alcance efectivo queda en `meta$orden_columnas`.
#' @param max_columnas_orden Máximo de columnas numéricas o temporales que se
#'   comparan entre sí para detectar relaciones de orden. Las columnas que
#'   exceden el límite se conservan en `meta$orden_columnas$columnas_omitidas`.
#' @param umbral_solapamiento_orden Solapamiento mínimo de los rangos
#'   intercuartiles para considerar que dos columnas representan magnitudes
#'   comparables. Por defecto es `0.1`: al menos una décima parte del rango
#'   intercuartílico más ancho debe ser común a ambos. Esto evita interpretar
#'   como restricción fila a fila un orden explicado sólo por escalas
#'   separadas. Si no hay ese solapamiento, una brecha con IQR exactamente cero
#'   conserva el par porque la mitad central sostiene el mismo desplazamiento
#'   fila a fila. No se aplica una tolerancia oculta. Use `0` para desactivar el
#'   filtro de magnitud. Ambos criterios se publican en la evidencia. Los pares
#'   descartados se cuentan en
#'   `meta$orden_columnas$pares_descartados_magnitud` y los recuperados en
#'   `meta$orden_columnas$pares_rescatados_brecha_estable`.
#' @param umbral_aritmetica Proporción mínima de filas comparables que deben
#'   satisfacer una identidad dentro de `tolerancia_aritmetica` para reconocer
#'   una regularidad aritmética entre columnas numéricas. El valor por omisión
#'   es `0.9`. Una vez reconocida la relación se informan todas sus
#'   discrepancias, sin un segundo filtro por su cantidad absoluta. La
#'   proporción y el criterio efectivos se publican en cada evidencia.
#' @param min_filas_aritmetica Mínimo de filas comparables necesario para
#'   evaluar una candidata aritmética. Por omisión es `3`.
#' @param tolerancia_aritmetica Tolerancia numérica relativa escalada usada al
#'   comparar un valor observado y uno esperado. Por omisión es `1e-8`; el
#'   criterio completo y el valor efectivo se declaran en cada evidencia.
#' @param max_columnas_aritmetica Máximo de columnas numéricas que intervienen
#'   en la búsqueda aritmética, cuyo costo crece cúbicamente. Por omisión es
#'   `20`. Si se omiten columnas, `cobertura_diagnosticos` declara el recorte y
#'   `meta$aritmetica_columnas` conserva los conteos de combinaciones.
#'
#' @return Objeto S3 de clase `perfil`. Cada fila de hallazgos incluye
#'   n_evaluados, n_afectados y unidad_conteo: son conteos de las unidades
#'   declaradas (por ejemplo fila, columna, formato o par). En
#'   `mayusculas_inconsistentes`, `normalizacion_unicode` y
#'   `casi_duplicados_vocabulario`, la unidad es
#'   `valor_distinto`: `n_evaluados` cuenta los valores distintos evaluados y
#'   `n_afectados` los valores distintos que participan en la colisión. Su
#'   `trazabilidad` sigue siendo por fila y enumera todas las filas que
#'   contienen esos valores, no sólo las filas defectuosas; por eso su total
#'   puede ser mayor que `n_afectados`. En `casi_duplicados_vocabulario`, la
#'   traza incluye todas las filas cuyos
#'   valores pertenecen al grupo elegido, incluida la forma dominante. La
#'   distancia es una senal heuristica, no una prueba de que cada fila deba
#'   corregirse; la evidencia declara cuantas filas mostradas pertenecen a las
#'   formas variantes y cuantas a las formas dominantes.
#'   En `filas_duplicadas`, el conteo y la traza incluyen todas las filas
#'   participantes de los grupos; el numero de excedentes queda en la
#'   evidencia. Cuando el camino
#'   no puede conocer un conteo, informa NA, nunca cero. La columna de lista
#'   `trazabilidad` distingue `disponible`, `truncada`, `no_aplica` y
#'   `no_disponible`; cuando corresponde conserva índices de fila acotados por
#'   `max_filas_hallazgo`, el total conocido y el alcance. En
#'   `casi_duplicados_vocabulario`, donde la traza mezcla filas de formas
#'   variantes con filas de la forma dominante, conserva además
#'   `n_filas_formas_variantes` y `n_filas_formas_dominantes` con el reparto
#'   completo, y `mostrados_formas_variantes` y `mostrados_formas_dominantes`
#'   con el reparto de lo que sobrevivió al truncado. Las variantes se entregan
#'   primero, de modo que el truncado no se lleve lo accionable. Para
#'   `patron_raro`,
#'   el alcance puede ser `completo`, `muestra_patrones`,
#'   `patrones_parciales` o `muestra_patrones+patrones_parciales`. El resumen y
#'   el texto de evidencia de `patron_raro` muestran como maximo seis patrones,
#'   pero la trazabilidad conserva los nombres de todos los patrones raros hasta
#'   un limite de 5.000; `patrones_parciales` indica que se alcanzo ese limite,
#'   no que se haya alcanzado el tope de presentacion.
#'   Cuando se emite un hallazgo `patron_raro`, su evidencia incluye la
#'   proporcion del patron dominante y cuantas filas pertenecen a patrones no
#'   dominantes que superan `umbral_patron_raro` y por eso quedan excluidos.
#'   Si el patron dominante no alcanza `umbral_patron_dominante`, no se emite
#'   el hallazgo: `cobertura_diagnosticos` declara la no medicion, su proporcion
#'   observada y el argumento que se puede ajustar.
#'   Si el conteo y la traza no coinciden, conserva el hallazgo y emite una
#'   advertencia de clase `lupa_trazabilidad_incoherente`. La guarda compara el
#'   total previo al truncado y respeta la unidad declarada.
#'   Una matriz no analizada conserva en la traza todas sus filas. Si una
#'   columna de listas se reconoce como constante pero no se puede contar su
#'   frecuencia, el conteo afectado queda en NA y `cobertura_diagnosticos`
#'   explica la no evaluación.
#'   Los índices no contienen valores. Usarlos para extraer filas de los datos
#'   originales puede volver a exponer datos personales; el paquete no realiza
#'   esa extracción y la protección de salidas no sustituye el control de acceso
#'   a los datos de entrada.
#'   En la evidencia de `casi_duplicados_vocabulario`, `clase_diferencia` puede
#'   ser `normalizacion_exacta` (la coincidencia aparece después de normalizar),
#'   `dentro_de_palabra` (la diferencia está dentro de un token),
#'   `token_completo` (cambian tokens completos), `token_unico` (ambos valores
#'   son un solo token y esa distinción estructural no aplica), `mixta` (el grupo
#'   reúne aristas de más de una clase) o `indeterminada` (no hubo aristas
#'   clasificables). Son categorías de evidencia, no veredictos de identidad.
#'   `cobertura_diagnosticos` es una tabla hermana de `hallazgos`, con una fila
#'   por diagnóstico que no pudo evaluarse o cuya enumeración quedó parcial y
#'   las columnas `diagnostico`,
#'   `columna`, `motivo`, `como_resolverlo` y `dependencia`. Incluye la falta de
#'   `stringdist`, `stringi`, `bit64` o `sf`, y las zonas horarias POSIXt sin
#'   declarar. Los patrones de frecuencia intermedia no se consideran desvios
#'   del patron dominante: `patron_raro` es completo respecto de su criterio de
#'   rareza cuando no hay recorte de trazabilidad. Si el conjunto de nombres
#'   raros supera 5.000, `cobertura_diagnosticos` declara el recorte y su limite.
#'   Quien decida automáticamente sobre un perfil debe revisar
#'   `nrow(perfil$cobertura_diagnosticos)` además de las severidades: un perfil
#'   sin hallazgos y con diagnósticos no evaluados no es un perfil limpio.
#' @export
#' @seealso [descubrir_patrones()], [detectar_dependencias()],
#'   [proponer_modelo()], [planificar_limpieza()]
#'
#' @examples
#' perfil <- perfilar(datos_administrativos)
#' perfil
#' summary(perfil)
perfilar <- function(datos,
                     nombre = deparse(substitute(datos)),
                     fecha = Sys.time(),
                     muestra = 1e5,
                     max_patrones = 20,
                     distinguir_mayusculas = TRUE,
                     expandir = FALSE,
                     umbral_alta_cardinalidad = 0.5,
                     umbral_faltantes_sospechoso = 0.1,
                     clave = NULL,
                     umbral_faltantes_error = 0.4,
                     umbral_patron_raro = 0.05,
                     umbral_patron_dominante = 0.5,
                     columnas_sin_ceros = character(),
                     columnas_no_negativas = character(),
                     columnas_opcionales = character(),
                     aplicabilidad = NULL,
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
                     tolerancia_aritmetica = 1e-8,
                     max_columnas_aritmetica = 20L,
                     casi_duplicados_vocabulario = TRUE,
                     max_proporcion_grupo_vocabulario = 0.5,
                     umbral_variante_rara_vocabulario = 0.05,
                     min_asimetria_vocabulario_corto = 10,
                     min_asimetria_vocabulario = 2,
                     min_participacion_dominante_vocabulario_corto = 0.5) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe ser un data.frame, tibble o data.table.", call. = FALSE)
  }
  normalizacion_resuelta <- .resolver_normalizacion(normalizar)
  fecha_hora <- tryCatch(.fecha_utc(fecha), error = function(e) NA)
  if (length(fecha_hora) != 1L || is.na(fecha_hora) ||
      !is.finite(as.numeric(fecha_hora))) {
    stop("`fecha` debe contener una fecha y hora v\u00e1lida.", call. = FALSE)
  }
  umbrales <- c(
    umbral_alta_cardinalidad, umbral_faltantes_sospechoso,
    umbral_faltantes_error, umbral_patron_raro, umbral_patron_dominante
  )
  if (anyNA(umbrales) || any(umbrales < 0 | umbrales > 1)) {
    stop("Todos los umbrales deben estar entre 0 y 1.", call. = FALSE)
  }
  if (umbral_faltantes_error < umbral_faltantes_sospechoso) {
    stop("El umbral de error no puede ser menor que el sospechoso.", call. = FALSE)
  }
  if (!is.null(clave)) {
    if (!is.character(clave) || !length(clave) || anyNA(clave) ||
        !all(nzchar(clave))) {
      stop(
        "`clave` debe ser un vector de nombres de columna sin NA.",
        call. = FALSE
      )
    }
    faltantes <- setdiff(clave, names(datos))
    if (length(faltantes)) {
      stop(
        "`clave` nombra columnas que no estan en los datos: ",
        paste(faltantes, collapse = ", "),
        ". Disponibles: ", paste(names(datos), collapse = ", "), ".",
        call. = FALSE
      )
    }
    if (anyDuplicated(clave)) {
      stop("`clave` repite una columna.", call. = FALSE)
    }
    # Una clave que no identifica una fila sirve igual para localizar, pero
    # deja de ser una clave: se avisa y se sigue, no se rompe.
    if (nrow(datos) && anyDuplicated(datos[, clave, drop = FALSE])) {
      repetidas <- sum(duplicated(datos[, clave, drop = FALSE]))
      cli::cli_warn(paste0(
        "La clave declarada no es unica: ", repetidas,
        " filas repiten su valor. La trazabilidad la usa igual para localizar, ",
        "pero un valor puede senalar mas de una fila."
      ))
    }
  }
  if (!is.numeric(sentinelas_numericos) || anyNA(sentinelas_numericos) ||
      any(!is.finite(sentinelas_numericos))) {
    stop("`sentinelas_numericos` debe ser un vector num\u00e9rico finito.", call. = FALSE)
  }
  if (!is.logical(analizar_dependencias) || length(analizar_dependencias) != 1L ||
      is.na(analizar_dependencias)) {
    stop("`analizar_dependencias` debe ser un l\u00f3gico escalar sin NA.",
         call. = FALSE)
  }
  for (argumento in c("datos_personales_permitidos", "proteger_datos_personales")) {
    valor <- get(argumento)
    if (!is.logical(valor) || length(valor) != 1L || is.na(valor)) {
      stop("`", argumento, "` debe ser TRUE o FALSE.", call. = FALSE)
    }
  }
  if (!is.numeric(umbral_documento_verificado) ||
      length(umbral_documento_verificado) != 1L ||
      is.na(umbral_documento_verificado) ||
      umbral_documento_verificado < 0 || umbral_documento_verificado > 1) {
    stop("`umbral_documento_verificado` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.numeric(muestra_validadores) || length(muestra_validadores) != 1L ||
      is.na(muestra_validadores) || muestra_validadores < 1 ||
      (!is.infinite(muestra_validadores) &&
       muestra_validadores != floor(muestra_validadores))) {
    stop("`muestra_validadores` debe ser un entero positivo o Inf.", call. = FALSE)
  }
  muestra_validadores <- if (is.infinite(muestra_validadores)) Inf else {
    as.integer(muestra_validadores)
  }
  if (!is.numeric(max_filas_hallazgo) || length(max_filas_hallazgo) != 1L ||
      is.na(max_filas_hallazgo) || max_filas_hallazgo < 1 ||
      (!is.infinite(max_filas_hallazgo) &&
       max_filas_hallazgo != floor(max_filas_hallazgo))) {
    stop("`max_filas_hallazgo` debe ser un entero positivo o Inf.", call. = FALSE)
  }
  max_filas_hallazgo <- if (is.infinite(max_filas_hallazgo)) {
    Inf
  } else as.integer(max_filas_hallazgo)
  if (!is.numeric(umbral_orden_columnas) ||
      length(umbral_orden_columnas) != 1L ||
      is.na(umbral_orden_columnas) || umbral_orden_columnas <= 0 ||
      umbral_orden_columnas > 1) {
    stop("`umbral_orden_columnas` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.numeric(max_columnas_orden) || length(max_columnas_orden) != 1L ||
      is.na(max_columnas_orden) || max_columnas_orden < 2 ||
      max_columnas_orden != floor(max_columnas_orden)) {
    stop("`max_columnas_orden` debe ser un entero de al menos 2.", call. = FALSE)
  }
  max_columnas_orden <- as.integer(max_columnas_orden)
  if (!is.numeric(umbral_solapamiento_orden) ||
      length(umbral_solapamiento_orden) != 1L ||
      is.na(umbral_solapamiento_orden) ||
      umbral_solapamiento_orden < 0 || umbral_solapamiento_orden > 1) {
    stop("`umbral_solapamiento_orden` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.numeric(umbral_aritmetica) || length(umbral_aritmetica) != 1L ||
      is.na(umbral_aritmetica) || umbral_aritmetica <= 0 ||
      umbral_aritmetica > 1) {
    stop("`umbral_aritmetica` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.numeric(min_filas_aritmetica) ||
      length(min_filas_aritmetica) != 1L ||
      is.na(min_filas_aritmetica) || !is.finite(min_filas_aritmetica) ||
      min_filas_aritmetica < 3 ||
      min_filas_aritmetica != floor(min_filas_aritmetica)) {
    stop("`min_filas_aritmetica` debe ser un entero de al menos 3.",
         call. = FALSE)
  }
  min_filas_aritmetica <- as.integer(min_filas_aritmetica)
  if (!is.numeric(tolerancia_aritmetica) ||
      length(tolerancia_aritmetica) != 1L ||
      is.na(tolerancia_aritmetica) || !is.finite(tolerancia_aritmetica) ||
      tolerancia_aritmetica < 0) {
    stop("`tolerancia_aritmetica` debe ser un n\u00famero finito no negativo.",
         call. = FALSE)
  }
  if (!is.numeric(max_columnas_aritmetica) ||
      length(max_columnas_aritmetica) != 1L ||
      is.na(max_columnas_aritmetica) || max_columnas_aritmetica < 2 ||
      max_columnas_aritmetica != floor(max_columnas_aritmetica)) {
    stop("`max_columnas_aritmetica` debe ser un entero de al menos 2.",
         call. = FALSE)
  }
  max_columnas_aritmetica <- as.integer(max_columnas_aritmetica)
  if (!is.logical(casi_duplicados_vocabulario) ||
      length(casi_duplicados_vocabulario) != 1L ||
      is.na(casi_duplicados_vocabulario)) {
    stop("`casi_duplicados_vocabulario` debe ser TRUE o FALSE.", call. = FALSE)
  }
  if (!is.numeric(max_proporcion_grupo_vocabulario) ||
      length(max_proporcion_grupo_vocabulario) != 1L ||
      is.na(max_proporcion_grupo_vocabulario) ||
      max_proporcion_grupo_vocabulario < 0 ||
      max_proporcion_grupo_vocabulario > 1) {
    stop("`max_proporcion_grupo_vocabulario` debe estar entre 0 y 1.",
         call. = FALSE)
  }
  if (!is.numeric(umbral_variante_rara_vocabulario) ||
      length(umbral_variante_rara_vocabulario) != 1L ||
      is.na(umbral_variante_rara_vocabulario) ||
      umbral_variante_rara_vocabulario < 0 ||
      umbral_variante_rara_vocabulario > 1) {
    stop("`umbral_variante_rara_vocabulario` debe estar entre 0 y 1.",
         call. = FALSE)
  }
  if (!is.numeric(min_asimetria_vocabulario_corto) ||
      length(min_asimetria_vocabulario_corto) != 1L ||
      is.na(min_asimetria_vocabulario_corto) ||
      !is.finite(min_asimetria_vocabulario_corto) ||
      min_asimetria_vocabulario_corto < 1) {
    stop("`min_asimetria_vocabulario_corto` debe ser al menos 1.",
         call. = FALSE)
  }
  if (!is.numeric(min_participacion_dominante_vocabulario_corto) ||
      length(min_participacion_dominante_vocabulario_corto) != 1L ||
      is.na(min_participacion_dominante_vocabulario_corto) ||
      min_participacion_dominante_vocabulario_corto < 0 ||
      min_participacion_dominante_vocabulario_corto > 1) {
    stop(paste0(
      "`min_participacion_dominante_vocabulario_corto` debe estar entre 0 y 1."
    ), call. = FALSE)
  }
  if (!is.logical(duplicados_aproximados) &&
      !is.list(duplicados_aproximados)) {
    stop("`duplicados_aproximados` debe ser FALSE, TRUE o una lista de argumentos.",
         call. = FALSE)
  }
  if (is.logical(duplicados_aproximados) &&
      (length(duplicados_aproximados) != 1L ||
       is.na(duplicados_aproximados))) {
    stop("`duplicados_aproximados` debe ser FALSE o TRUE.", call. = FALSE)
  }
  if (is.list(duplicados_aproximados) &&
      any(names(duplicados_aproximados) %in% c(
        "datos", "clasificacion", "perfil", "proteger_datos_personales",
        "normalizar"
      ))) {
    stop("`duplicados_aproximados` no puede reemplazar argumentos coordinados por perfilar().",
         call. = FALSE)
  }
  validadores_personales <- .normalizar_validadores_personales(
    validadores_personales
  )

  nombres <- names(datos)
  if (is.null(nombres)) {
    nombres <- paste0("V", seq_len(ncol(datos)))
  }
  nombres_lista <- make.unique(nombres)
  aplicabilidad_resuelta <- .resolver_aplicabilidad(
    datos, nombres, columnas_opcionales, aplicabilidad
  )
  resultados <- lapply(seq_len(ncol(datos)), function(i) {
    .perfilar_columna(
      datos[[i]], nombres[[i]], muestra, max_patrones,
      distinguir_mayusculas, expandir, umbral_patron_raro,
      sentinelas_numericos,
      aplicable = aplicabilidad_resuelta$mascaras[[i]]
    )
  })

  columnas <- if (length(resultados)) {
    do.call(rbind, lapply(resultados, `[[`, "fila"))
  } else {
    .perfilar_columna(
      character(), "", muestra, max_patrones,
      distinguir_mayusculas, expandir, umbral_patron_raro,
      sentinelas_numericos
    )$fila[0, , drop = FALSE]
  }
  rownames(columnas) <- NULL
  patrones <- lapply(resultados, `[[`, "patrones")
  formatos_fecha <- lapply(resultados, `[[`, "formatos")
  names(patrones) <- nombres_lista
  names(formatos_fecha) <- nombres_lista
  dependencias <- if (analizar_dependencias) {
    detectar_dependencias(
      datos, umbral = umbral_dependencia, muestra = muestra,
      max_columnas = max_columnas_dependencias,
      umbral_casi_clave = umbral_casi_clave_dependencia
    )
  } else {
    detectar_dependencias(
      datos[0, 0, drop = FALSE], umbral = umbral_dependencia,
      max_columnas = 1L,
      umbral_casi_clave = umbral_casi_clave_dependencia
    )
  }

  n_filas_duplicadas <- tryCatch(
    sum(duplicated(datos)),
    error = function(e) NA_integer_
  )
  n_filas_en_grupos_duplicados <- tryCatch(
    sum(duplicated(datos) | duplicated(datos, fromLast = TRUE)),
    error = function(e) NA_integer_
  )
  filas_completas <- tryCatch(
    sum(stats::complete.cases(datos)),
    error = function(e) NA_integer_
  )
  duplicadas <- .columnas_duplicadas(datos, nombres)
  relaciones_orden <- .detectar_orden_columnas(
    datos, columnas, resultados, formatos_fecha,
    umbral = umbral_orden_columnas, max_columnas = max_columnas_orden,
    umbral_solapamiento = umbral_solapamiento_orden
  )
  relaciones_aritmeticas <- .detectar_aritmetica_columnas(
    datos, umbral = umbral_aritmetica,
    min_filas = min_filas_aritmetica,
    tolerancia = tolerancia_aritmetica,
    max_columnas = max_columnas_aritmetica
  )
  normalizacion_fusiones <- .normalizacion_fusiones_tabla(
    datos, normalizacion_resuelta
  )
  tipos <- table(vapply(seq_along(datos), function(i) {
    .tipo_declarado(datos[[i]])
  }, character(1L)))
  tipos_columnas <- data.frame(
    tipo = names(tipos), n = as.integer(tipos), stringsAsFactors = FALSE
  )
  general <- list(
    filas = nrow(datos),
    columnas = ncol(datos),
    celdas = as.numeric(nrow(datos)) * as.numeric(ncol(datos)),
    memoria_bytes = as.numeric(utils::object.size(datos)),
    filas_completas = filas_completas,
    filas_duplicadas = n_filas_duplicadas,
    filas_en_grupos_duplicados = n_filas_en_grupos_duplicados,
    tipos_columnas = tipos_columnas,
    columnas_duplicadas = duplicadas
  )
  hallazgos <- .construir_hallazgos(
    datos, resultados, nombres, duplicadas,
    umbral_alta_cardinalidad, umbral_faltantes_sospechoso,
    umbral_faltantes_error, umbral_patron_raro,
    umbral_patron_dominante, columnas_sin_ceros,
    columnas_no_negativas,
    if (is.na(n_filas_duplicadas)) 0L else n_filas_duplicadas,
    if (is.na(n_filas_en_grupos_duplicados)) 0L else
      n_filas_en_grupos_duplicados,
    relaciones_orden = relaciones_orden$hallazgos,
    relaciones_aritmeticas = relaciones_aritmeticas$hallazgos,
    normalizacion = normalizacion_resuelta,
    detectar_casi_duplicados = casi_duplicados_vocabulario,
    max_proporcion_grupo = max_proporcion_grupo_vocabulario,
    umbral_variante_rara = umbral_variante_rara_vocabulario,
    min_asimetria_variante = min_asimetria_vocabulario_corto,
    min_asimetria_general = min_asimetria_vocabulario,
    min_participacion_dominante =
      min_participacion_dominante_vocabulario_corto
  )
  cobertura_diagnosticos <- attr(
    hallazgos, "cobertura_diagnosticos", exact = TRUE
  )
  if (is.null(cobertura_diagnosticos)) {
    cobertura_diagnosticos <- .cobertura_diagnosticos_vacia()
  }
  if (nrow(relaciones_aritmeticas$cobertura)) {
    cobertura_diagnosticos <- rbind(
      cobertura_diagnosticos, relaciones_aritmeticas$cobertura
    )
  }
  cobertura_dependencias <- .cobertura_dependencias(dependencias)
  if (!is.null(cobertura_dependencias)) {
    cobertura_diagnosticos <- rbind(
      cobertura_diagnosticos, cobertura_dependencias
    )
  }
  cobertura_aplicabilidad <- .cobertura_aplicabilidad(aplicabilidad_resuelta$reglas)
  if (!is.null(cobertura_aplicabilidad)) {
    cobertura_diagnosticos <- rbind(
      cobertura_diagnosticos, cobertura_aplicabilidad
    )
  }
  attr(hallazgos, "cobertura_diagnosticos") <- NULL
  benford <- .diagnosticar_benford(datos, columnas, hallazgos)
  if (nrow(benford$cobertura)) {
    cobertura_diagnosticos <- rbind(
      cobertura_diagnosticos, benford$cobertura
    )
  }
  if (length(benford$hallazgos)) {
    hallazgos <- do.call(rbind, c(list(hallazgos), benford$hallazgos))
    hallazgos$severidad <- factor(
      as.character(hallazgos$severidad),
      levels = c("ok", "sospechoso", "error"), ordered = TRUE
    )
    rownames(hallazgos) <- NULL
  }
  datos_personales <- .detectar_datos_personales(
    datos, nombres, resultados,
    validadores = validadores_personales,
    umbral_verificado = umbral_documento_verificado,
    muestra_validadores = muestra_validadores
  )
  indice_personal <- match(columnas$columna, datos_personales$columna)
  columnas$dato_personal_posible <- !is.na(indice_personal)
  columnas$tipo_dato_personal <- datos_personales$tipo[indice_personal]
  columnas$proporcion_dato_personal <-
    datos_personales$proporcion_compatible[indice_personal]
  columnas$poder_discriminante_dato_personal <-
    datos_personales$poder_discriminante[indice_personal]
  columnas$dato_personal_protegido <- ifelse(
    is.na(indice_personal), FALSE, datos_personales$proteger[indice_personal]
  )
  hallazgos_personales <- .hallazgos_datos_personales(
    datos_personales, datos_personales_permitidos
  )
  hallazgos_personales <- c(
    hallazgos_personales,
    .hallazgos_rango_nacimiento(columnas, datos_personales, fecha_hora)
  )
  if (length(hallazgos_personales)) {
    hallazgos <- do.call(rbind, c(list(hallazgos), hallazgos_personales))
    hallazgos$severidad <- factor(
      as.character(hallazgos$severidad),
      levels = c("ok", "sospechoso", "error"), ordered = TRUE
    )
    rownames(hallazgos) <- NULL
  }
  aproximados <- if (is.logical(duplicados_aproximados) &&
      !duplicados_aproximados) {
    NULL
  } else {
    configuracion <- if (isTRUE(duplicados_aproximados)) {
      list()
    } else duplicados_aproximados
    do.call(
      .detectar_duplicados_aproximados,
      c(
        list(
          datos = datos, clasificacion = datos_personales,
          normalizar = normalizacion_resuelta,
          proteger_datos_personales = proteger_datos_personales,
          fusiones_precomputadas = normalizacion_fusiones
        ),
        configuracion
      )
    )
  }
  if (!is.null(aproximados) && nrow(aproximados$hallazgos)) {
    hallazgos <- rbind(hallazgos, aproximados$hallazgos)
    rownames(hallazgos) <- NULL
  }
  hallazgos <- .agregar_trazabilidad_hallazgos(
    hallazgos, datos, nombres, resultados, expandir = expandir,
    aproximados = aproximados, limite = max_filas_hallazgo,
    distinguir_mayusculas = distinguir_mayusculas, clave = clave
  )
  meta <- list(
    nombre = nombre,
    fecha_hora = fecha_hora,
    version = .version_paquete(),
    filas_totales = nrow(datos),
    filas_analizadas = min(nrow(datos), floor(muestra)),
    muestreo = nrow(datos) > muestra,
    muestra = muestra,
    max_patrones = max_patrones,
    distinguir_mayusculas = distinguir_mayusculas,
    expandir = expandir,
    umbral_patron_raro = umbral_patron_raro,
    analizar_dependencias = analizar_dependencias,
    umbral_dependencia = umbral_dependencia,
    umbral_casi_clave_dependencia = umbral_casi_clave_dependencia,
    max_columnas_dependencias = max_columnas_dependencias,
    sentinelas_numericos = .numeros_na(sentinelas_numericos),
    datos_personales_permitidos = datos_personales_permitidos,
    proteger_datos_personales = proteger_datos_personales,
    validadores_personales = names(validadores_personales),
    umbral_documento_verificado = umbral_documento_verificado,
    muestra_validadores = muestra_validadores,
    max_filas_hallazgo = max_filas_hallazgo,
    umbral_orden_columnas = umbral_orden_columnas,
    max_columnas_orden = max_columnas_orden,
    umbral_solapamiento_orden = umbral_solapamiento_orden,
    casi_duplicados_vocabulario = casi_duplicados_vocabulario,
    max_proporcion_grupo_vocabulario = max_proporcion_grupo_vocabulario,
    umbral_variante_rara_vocabulario =
      umbral_variante_rara_vocabulario,
    min_asimetria_vocabulario_corto = min_asimetria_vocabulario_corto,
    min_participacion_dominante_vocabulario_corto =
      min_participacion_dominante_vocabulario_corto,
    orden_columnas = relaciones_orden$alcance,
    normalizacion = normalizacion_resuelta,
    normalizacion_resumen = .normalizacion_resumen(normalizacion_resuelta),
    normalizacion_fusiones = normalizacion_fusiones
  )
  if (!is.null(benford$meta)) {
    meta$benford <- benford$meta
  }
  if (length(relaciones_aritmeticas$hallazgos) ||
      isTRUE(relaciones_aritmeticas$alcance$truncado)) {
    meta$aritmetica_columnas <- relaciones_aritmeticas$alcance
  }
  estructura <- list(
    general = general,
    columnas = columnas,
    patrones = patrones,
    formatos_fecha = formatos_fecha,
    dependencias = dependencias,
    hallazgos = hallazgos,
    cobertura_diagnosticos = cobertura_diagnosticos,
    datos_personales = datos_personales,
    meta = meta
  )
  if (!is.null(aproximados)) {
    estructura$duplicados_aproximados <- aproximados
    estructura <- estructura[c(
      "general", "columnas", "patrones", "formatos_fecha", "dependencias",
      "duplicados_aproximados", "hallazgos", "cobertura_diagnosticos",
      "datos_personales", "meta"
    )]
  }
  class(estructura) <- "perfil"
  if (proteger_datos_personales) estructura <- .proteger_perfil(estructura)
  estructura
}
