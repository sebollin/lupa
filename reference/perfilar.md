# Perfilar un conjunto de datos

Examina un `data.frame`, `tibble` o `data.table` y devuelve estadísticas
generales, métricas por columna, patrones, formatos de fecha y hallazgos
accionables. Todas las proporciones se expresan en `[0, 1]`.

## Usage

``` r
perfilar(
  datos,
  nombre = .nombre_de_los_datos(substitute(datos)),
  fecha = Sys.time(),
  muestra = 1e+05,
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
  ausencia_estructural = TRUE,
  sentinelas_numericos = c(-9, -99, -999, -9999, 999),
  cadenas_ausencia = NULL,
  analizar_dependencias = TRUE,
  umbral_dependencia = 0.995,
  umbral_casi_clave_dependencia = 0.8,
  max_columnas_dependencias = 100L,
  datos_personales_permitidos = TRUE,
  proteger_datos_personales = TRUE,
  columnas_personales = character(),
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
  min_asimetria_vocabulario = 2,
  min_participacion_dominante_vocabulario_corto = 0.5,
  variantes_equifrecuentes_vocabulario = FALSE,
  max_asimetria_equifrecuente_vocabulario = 2,
  max_comparaciones_dependencias = 200000L,
  max_trabajo_vocabulario = 2e+10,
  max_trabajo_dependencias = 1e+08,
  max_largo_valor_vocabulario = .MAX_LARGO_VALOR_CASI_DUPLICADOS,
  max_celdas_muestra = .MAX_CELDAS_MUESTRA,
  max_bytes_muestra = .MAX_BYTES_MUESTRA,
  avisar_costo_tabla_ancha = TRUE,
  umbral_celdas_aviso_tabla_ancha = .UMBRAL_CELDAS_AVISO_TABLA_ANCHA
)
```

## Arguments

- datos:

  Objeto que hereda de `data.frame` —`tibble` y `data.table` entran por
  ahí— o una matriz de dos dimensiones, que se convierte con
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html). La
  conversión queda declarada en `meta$entrada_convertida` para que el
  perfil no aparente haber recibido lo que no recibió.

- nombre:

  Nombre descriptivo del objeto.

- fecha:

  Fecha y hora de la corrida. Se puede fijar para construir series
  reproducibles; se normaliza a UTC.

- muestra:

  Máximo de filas usadas para patrones, inferencia de tipos, formatos de
  fecha y dependencias funcionales. Use `Inf` para analizar todas las
  filas en esos análisis.

- max_patrones:

  Máximo de patrones mostrados por columna.

- distinguir_mayusculas:

  Si se distinguen mayúsculas y minúsculas.

- expandir:

  Si se emite un token por carácter en los patrones.

- umbral_alta_cardinalidad:

  Umbral sobre la tasa de valores distintos de una columna categórica.
  No alcanza por sí solo: el hallazgo exige además al menos diez valores
  distintos, porque con pocos la tasa está dominada por el tamaño de la
  tabla —dos valores en tres filas dan 0,67 y superan cualquier umbral
  razonable— y una columna de dos valores no puede tener cardinalidad
  alta.

- umbral_faltantes_sospechoso:

  Umbral inferior de faltantes. El hallazgo se activa al superarlo en
  sentido estricto.

- clave:

  Nombres de las columnas que identifican una fila. Cuando se declaran,
  la trazabilidad de cada hallazgo trae además el valor de esas columnas
  para las filas señaladas, de modo que el caso se pueda verificar en el
  sistema de origen sin abrir la tabla. El índice de fila se conserva
  siempre. La clave declarada se trata como sensible: si la protección
  de datos personales está activa y alguna de esas columnas se clasifica
  como personal, sus valores salen enmascarados igual que la evidencia.
  La comprobación de una clave tiene dos ejes independientes:
  `meta$clave$unicidad` comprueba si sus combinaciones son únicas con la
  semántica de R, y `meta$clave$ausencia_nulos` comprueba que sus
  componentes no tengan valores ausentes. Cada eje declara `verificada`,
  `refutada` o `no_verificada`; una clave con ambos ejes verificados
  conserva exactamente el objeto histórico y no agrega metadatos. Cuando
  un eje falla o no se puede comprobar, `meta$clave$trazabilidad`
  explica que la localización agrupa con la semántica de R, incluso si
  el motor SQL trata dos `NULL` como distintos. `hallazgos` separa esos
  ejes: `clave_con_ausentes` enumera las filas que impiden la garantía
  `NOT NULL`, mientras `clave_no_unica` sólo informa repeticiones entre
  filas con la clave completa, el mismo universo que
  `meta$clave$unicidad`.

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

- columnas_opcionales:

  Nombres de columnas donde la ausencia no es un defecto. Su universo de
  completitud son las celdas presentes, y `cobertura_diagnosticos`
  declara el recorte. Sirve para el vacío por diseño: un historial con
  vigencia abierta, una columna que sólo corresponde a algunas filas.

- aplicabilidad:

  Lista con nombre por columna, donde cada elemento es una fórmula de un
  solo lado evaluada sobre `datos` — por ejemplo
  `list(marca_auto = ~ tiene_auto == "Si")`. Las filas donde el
  predicado no se cumple salen del universo de esa columna: no cuentan
  como ausencia. Las filas donde el predicado no se puede determinar se
  declaran aparte, sin contarse ni como aplicables ni como no
  aplicables. Un valor presente fuera del universo produce el hallazgo
  `valor_fuera_de_aplicabilidad`, que es el error simétrico y hoy no
  tiene otra forma de aparecer.

  `lupa` no infiere el universo: si nadie lo declara, toda la columna
  aplica y el resultado es el de siempre. Declararlo es lo que distingue
  el vacío por diseño del vacío por error, y sin esa distinción una
  tabla sana puede informar completitud baja siendo completa.

  **Hasta dónde llega el universo.** Gobierna todo el perfilado: los
  conteos, las proporciones, los hallazgos y las acciones que propone
  [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md).
  Medido sobre una columna condicionada de mil filas con universo de
  trescientas, treinta de ellas vacías: declarándolo, la proporción de
  faltantes es `0,100`, no hay hallazgo y el plan no propone nada; sin
  declararlo son `0,730`, sale `faltantes` y el plan propone dos
  acciones.

  El universo tambien llega a la limpieza. El perfil guarda la regla, el
  plan la lleva y
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  no toca las celdas que quedan fuera: una `S/D` en una fila donde la
  columna no corresponde no se convierte en ausencia, y las acciones que
  marcan o eliminan filas solo consideran ese universo. Si la regla da
  un valor indeterminado, la celda tampoco se toca. Las conversiones de
  tipo son la excepción, porque una columna tiene un solo tipo y no se
  puede convertir a medias: el plan declara en `n_afectadas` y en la
  justificación que cambia la columna entera. Si el plan se aplica a
  otros datos donde la regla no se puede evaluar, la acción falla y lo
  dice, en vez de operar sobre toda la columna.

  La medición contra un marco recibe la misma declaración:
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) acepta
  `aplicabilidad` y recorta las filas antes de medir, así que el
  histórico y la deriva —que consumen mediciones, no perfiles— heredan
  el número correcto. El `aplicabilidad` de
  [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
  es otra cosa: dice si un factor aplica a datos temporales o
  geométricos, no qué filas entran al universo.

- ausencia_estructural:

  Si se busca evidencia de que la ausencia de una columna es por diseño.
  `lupa` no infiere el universo ni lo cambia por su cuenta, pero
  declarar `aplicabilidad` exige saber que existe: quien perfila una
  tabla con columnas condicionadas sin declarar nada recibe el mismo
  informe engañoso que si el vacío fuera un defecto. Con `TRUE`, cuando
  el valor de otra columna decide qué filas tienen ésta, o cuando dos
  columnas se reparten las filas sin pisarse, se emite
  `posible_ausencia_estructural` con severidad `ok`, la evidencia medida
  y la línea exacta que habría que escribir para declararlo. Sugiere; no
  decide. Las columnas ya declaradas quedan fuera del examen. La regla
  tiene que cumplirse en al menos el 99 % de las filas: una que un solo
  caso rompe por debajo de ese corte no se publica, porque ofrecer la
  línea para declararla sería afirmar de más.

  Con el mismo argumento viaja `regla_silencia_ausencia`, también `ok`:
  avisa cuando una columna declarada opcional o con universo propio
  sigue casi vacía dentro de ese universo. La declaración funcionó y por
  eso el perfil salió limpio; el aviso existe para que eso sea una
  decisión y no un efecto.

- sentinelas_numericos:

  Vector completo de valores numéricos que se interpretan como ausencia.
  Se normaliza como un conjunto numérico, por lo que el orden, el tipo
  entero o doble y los duplicados no cambian el resultado.
  [`numeric()`](https://rdrr.io/r/base/numeric.html) los desactiva; las
  cadenas de ausencia se siguen evaluando por separado.

  **La lista por omisión es una sospecha del paquete y se aplica con una
  reserva**: un valor de esa lista que aparece **una sola vez** y cae
  **dentro** del rango de los demás valores de la columna no se señala,
  porque una aparición única es un número y no una convención de
  ausencia. Un `999` suelto dentro de un catálogo de `1` a `3000` no se
  acusa; el mismo `999` repetido, o un `-9` fuera del rango, sí.

  La reserva **sólo** rige para la lista por omisión. Si usted declara
  sus propios valores —o pide
  [sentinelas_naniar](https://sebollin.github.io/lupa/reference/sentinelas_naniar.md),
  que es optar por una heurística más ancha—, se cuentan todos, incluida
  la aparición única. Ésa es también la forma de recuperar el caso que
  la reserva calla: declarar el valor lo vuelve a contar y además lo
  saca de los estadísticos del resumen.

- cadenas_ausencia:

  Cadenas que el usuario declara como ausencia, además de la lista
  incorporada. Lo declarado atraviesa la guarda del vocabulario: `sd`,
  `nc` y `nd` sólo cuentan solas en columnas de pocos valores distintos
  —para no acusar a un código de dos letras—, y declararlas las hace
  contar siempre. Es el equivalente textual de `sentinelas_numericos`.

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
  aplica además su propia protección predeterminada. El enmascarado es
  **por columna**: se reemplaza lo que describe a una columna protegida
  —su moda, sus estadísticos, sus ejemplos y la evidencia de sus
  hallazgos—, no todas las apariciones de ese texto en la salida. Un
  valor puede ser vocabulario compartido: `"S/D"` es un centinela de una
  cédula y a la vez el «sin dato» de `sexo`, y publicarlo en `sexo` no
  revela nada de la cédula. Lo que **no** tiene columna a la que
  atribuirse sí se enmascara en toda la salida: un hallazgo de filas
  duplicadas muestra filas enteras, y los componentes que describen la
  tabla —`general`, la cobertura, los formatos de fecha— no son de
  ninguna columna en particular. Los **nombres de columna** nunca se
  enmascaran, aunque un valor protegido aparezca dentro de uno: un
  nombre es estructura, existía antes y aparte del dato, y el paquete lo
  necesita para cruzar el perfil con los datos. Se enmascaraban, y en
  una base real `fecha_nacimiento` salió publicada como
  `fecha_[valor protegido]`:
  [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  y
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  abortaban porque los nombres del perfil ya no coincidían con los de
  los datos. Un campo se reconoce como de nombres por su **contenido**
  —todos sus valores son nombres de la entrada, solos o unidos—, así que
  la protección de los valores no se afloja: si un valor protegido es
  igual a un nombre de columna, su moda sigue tapada. Lo mismo vale para
  el **vocabulario**: un tipo de hallazgo, una severidad, una
  estrategia, el nombre de un diagnóstico o de una métrica, o el de un
  factor de un marco que declaró el usuario, no se enmascaran aunque
  coincidan con un valor protegido o lo contengan. El principio es el de
  los nombres de columna: son estructura, no salen de las celdas, y
  publicarlos no dice nada de ninguna fila —el tipo `constante` aparece
  porque otra columna es constante, no porque alguien se apellide así—.
  Se enmascaraban: en una base real, un nombre de persona contenido en
  `faltantes_disfrazados` dejó seis hallazgos con el tipo
  `[valor protegido]`, y el plan sin ninguna acción para ellos. La regla
  es por campo y por contenido: un campo cuyos valores son todos
  vocabulario del propio objeto queda afuera, uno que mezcla no. Ese
  alcance tiene un **piso**: un valor de una columna protegida que
  identifique —seis caracteres o más, el mismo corte con el que la
  batería de fugas decide qué cuenta como filtración— no se publica en
  ninguna parte, tenga o no columna a la que atribuirse, y también en
  los campos numéricos. Sin ese piso, una columna que el clasificador no
  marcó publicaba los documentos de la protegida con sólo repetirlos:
  una copia con nombre neutro, un texto libre que los contuviera, o los
  estadísticos de orden de una columna clasificada con poder
  discriminante `debil`. El vocabulario corto no entra en el piso:
  `"S/D"` sigue publicándose donde describe a una columna que no es
  personal. El piso tiene **una excepción declarada**, y es la única:
  [`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md)
  agrupando *por* una columna personal publica sus valores como
  etiquetas de grupo, porque la etiqueta es el eje del resultado y sin
  ella los grupos no se distinguen. Lo mismo cuando la columna de
  agrupación no es personal pero alguna etiqueta contiene un valor
  protegido de otra columna. No ocurre en silencio —avisa al ejecutar y
  lo declara en `etiquetas_personales`— y el remedio es agrupar por una
  columna seudonimizada. El piso alcanza además la **forma sin
  separadores**: `"771.771-01"` es el mismo documento que `"77177101"`,
  y la celda entera se enmascara. Las tildes y la caja también son
  cosméticas: `juan.perez` se enmascara frente al nombre protegido «Juan
  Pérez», y el nombre pegado a otras letras dentro de un correo o un
  alias, también. Vale también fuera del latín occidental —el vietnamita
  escrito sin sus marcas, el griego, el cirílico y el armenio en otra
  caja, el ancho completo— y con el valor en otra codificación: lo que
  no la declara y no es UTF-8 válido se lee como latin1 al comparar. En
  la **prosa del paquete** —descripción, sugerencia, motivo, cómo
  resolverlo, justificación— se exige además que la variante no tenga
  una letra pegada antes o después: con los nombres de millones de
  personas como valores protegidos, alguno aparecía cruzando palabras de
  esas frases y las tapaba enteras, también en columnas no personales.
  Lo que esa prosa cita entre comillas dobles —el nivel de un
  determinante en la sugerencia de `posible_ausencia_estructural`— no es
  prosa sino un valor: se compara con la regla de los valores y se tapa
  sólo la cita, para que la sugerencia se siga leyendo. Y si el
  determinante es una columna protegida, la sugerencia no reproduce el
  criterio. Y lo aplican también
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  y
  [`distribucion_valores()`](https://sebollin.github.io/lupa/reference/distribucion_valores.md),
  que antes protegían sólo por columna y publicaban lo que `perfilar()`
  tapaba sobre la misma tabla.

- columnas_personales:

  Columnas que traen datos personales, declaradas por quien conoce el
  dato. `c("cod_benef", "apodo")` las nombra sin decir de qué tipo son;
  `c(cod_benef = "documento_identidad")` además lo dice. Lo declarado
  gana sobre lo inferido y no se vuelve a examinar.

  Existe porque el léxico de nombres de columna **no puede ser
  completo**: una columna con documentos se puede llamar de cualquier
  manera, y ninguna lista de nombres frecuentes la va a reconocer. Es el
  mismo patrón que `columnas_opcionales`, aplicado a la otra decisión
  que el paquete no puede tomar solo.

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
  `meta$orden_columnas$pares_rescatados_brecha_estable`. Los pares que
  involucran una columna cuyo tipo o formato se descubrió sobre una
  muestra y cuya conversión dejó valores afuera no se comparan; el
  alcance los conserva en `columnas_conversion_parcial`,
  `n_valores_excluidos_conversion` y
  `pares_descartados_conversion_parcial`.

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
  bloque que abarque casi toda la columna. El diagnóstico se ejecuta por
  columna y no deduplica columnas con el mismo vocabulario: cada una
  conserva su propia fila de cobertura y sus propios hallazgos. El costo
  de comparar vocabularios idénticos puede repetirse.

- umbral_variante_rara_vocabulario:

  Proporcion maxima de la columna que puede ocupar una variante breve
  para abrir la comparacion por una edicion.

- min_asimetria_vocabulario_corto:

  Razon minima entre la frecuencia de una forma dominante y una variante
  breve para abrir la comparacion por una edicion.

- min_asimetria_vocabulario:

  Razón mínima entre la frecuencia de la forma dominante y la de la
  variante para abrir un grupo por la vía general de distancia. Por
  omisión `2`. Una asimetría de `1,5` significa que las dos formas son
  casi igual de comunes, que es evidencia muy floja de una errata:
  medido sobre tablas limpias y sobre erratas sembradas, los falsos
  positivos están entre `1,0` y `1,5` y las erratas reales desde `9,0`.

- min_participacion_dominante_vocabulario_corto:

  Proporcion minima de la columna que debe ocupar la forma dominante en
  la comparacion por una edicion.

- variantes_equifrecuentes_vocabulario:

  Si se informa el diagnóstico `variantes_equifrecuentes_vocabulario`,
  que señala dos formas cercanas que se reparten la columna sin que
  ninguna sea dominante —el error sistemático de dos operadores, una
  plantilla rota o una migración parcial—.

  **Está apagado por omisión y la razón está medida**: sobre la batería
  de 31 tablas limpias produce un grupo sospechoso donde no hay defecto.
  Lo que lo dispara no es el tamaño de la tabla sino el reparto: dos
  formas parecidas con frecuencias del mismo orden lo activan igual con
  cuatro filas que con quinientas, y en una tabla chica ese reparto sale
  por casualidad más seguido. Decía «tablas de menos de veinte filas», y
  se midió de cuatro a quinientas: dispara en todas. Es aditivo:
  encenderlo no cambia ni pierde ninguna detección de
  `casi_duplicados_vocabulario`. El límite que deja de cubrir se declara
  igual, encendido o apagado, en `n_grupos_sin_variante_rara`.

- max_asimetria_equifrecuente_vocabulario:

  Razón máxima entre la frecuencia mayor y la menor para considerar que
  dos formas se reparten la columna. Con `2`, `40` contra `5` no entra y
  `5` contra `5` sí.

- max_comparaciones_dependencias:

  Tope de pares determinante-dependiente que se comparan. El costo de
  las dependencias es del orden de `columnas^2 x filas` y empeora con
  determinantes casi únicos; cuando el presupuesto se agota, lo
  comparado se informa y lo que quedó sin comparar se declara en
  `cobertura_diagnosticos`, nunca como cero.

- max_trabajo_vocabulario:

  Tope de comparaciones de carácter para las comparaciones de distancia
  del vocabulario. Comparar dos valores cuesta del orden del producto de
  sus largos, así que el trabajo es la suma de ese producto sobre todos
  los pares. Contar pares por longitud media subestima las cadenas
  largas: medido, el mismo presupuesto compraba 5,3 millones de unidades
  por segundo con valores de 900 caracteres y 44 millones con valores
  de 40. Se combina con el límite interno de pares del detector y manda
  el más restrictivo; ese límite interno no es un argumento de
  `perfilar()`. El recorte declara valores, pares y trabajo sin
  comparar. `Inf` lo desactiva.

- max_trabajo_dependencias:

  Tope predeterminado de unidades fila-par para las dependencias. Se
  combina con `max_comparaciones_dependencias`; el límite efectivo baja
  cuando hay muchas filas. `Inf` desactiva este tope, pero no el de
  pares.

- max_largo_valor_vocabulario:

  Maximo de caracteres permitido en cada valor que entra en la
  comparacion de casi duplicados del vocabulario. Por defecto es
  `10000`, elegido porque la distancia normalizada deja de distinguir de
  forma estable una diferencia local de muchas diferencias en textos
  largos. Una columna que supera el tope se declara completa fuera de
  alcance en `cobertura_diagnosticos`; no se recortan valores en
  silencio. `Inf` recupera explicitamente el comportamiento anterior sin
  tope.

- max_celdas_muestra:

  Maximo de celdas que puede contener la muestra comun de los
  diagnosticos que muestrean filas. Por defecto es `1000000`; se calcula
  como filas efectivas por columnas de la tabla. Si reduce la muestra,
  `cobertura_diagnosticos` informa las filas y celdas solicitadas, el
  umbral y el nuevo alcance. `Inf` desactiva este tope.

- max_bytes_muestra:

  Maximo de bytes de la muestra materializada que alimenta los
  diagnosticos que muestrean filas. Por defecto es `512 MiB`. El tamaño
  se estima sobre una sonda de hasta cien filas y se comprueba sobre la
  muestra efectiva; si la cota reduce el alcance,
  `cobertura_diagnosticos` informa los bytes observados y el umbral.
  `Inf` desactiva este tope.

- avisar_costo_tabla_ancha:

  Si es `TRUE`, avisa en sesiones interactivas cuando las celdas
  proyectadas superan `umbral_celdas_aviso_tabla_ancha`. El aviso es una
  estimacion y queda en silencio en scripts no interactivos.

- umbral_celdas_aviso_tabla_ancha:

  Cantidad de celdas a partir de la cual se emite el aviso de tabla
  ancha. Por defecto, `100000`; `Inf` lo desactiva explicitamente.

## Value

Objeto S3 de clase `perfil`. Cada fila de hallazgos incluye n_evaluados,
n_afectados y unidad_conteo: son conteos de las unidades declaradas (por
ejemplo fila, columna, formato o par). En `mayusculas_inconsistentes`,
`normalizacion_unicode` y `casi_duplicados_vocabulario`, la unidad es
`valor_distinto`: `n_evaluados` cuenta los valores distintos evaluados y
`n_afectados` los valores distintos que participan en la colisión. Su
`trazabilidad` sigue siendo por fila y enumera todas las filas que
contienen esos valores, no sólo las filas defectuosas; por eso su total
puede ser mayor que `n_afectados`. En `casi_duplicados_vocabulario`, la
traza incluye todas las filas cuyos valores pertenecen al grupo elegido,
incluida la forma dominante. La distancia es una senal heuristica, no
una prueba de que cada fila deba corregirse; la evidencia declara
cuantas filas mostradas pertenecen a las formas variantes y cuantas a
las formas dominantes. En `filas_duplicadas`, el conteo y la traza
incluyen todas las filas participantes de los grupos; el numero de
excedentes queda en la evidencia. Cuando el camino no puede conocer un
conteo, informa NA, nunca cero. La columna de lista `trazabilidad`
distingue `disponible`, `truncada`, `no_aplica` y `no_disponible`;
cuando corresponde conserva índices de fila acotados por
`max_filas_hallazgo`, el total conocido y el alcance. En
`clave_con_ausentes`, cuenta las filas con al menos un componente
ausente y su traza enumera esas filas. En `clave_no_unica`, cuenta las
filas que participan en colisiones entre claves completas y su traza
excluye las filas incompletas; ambas unidades son `fila`. Así, una
colisión entre ausentes no aparece como una repetición que refute la
unicidad. `casi_duplicados_vocabulario`, donde la traza mezcla filas de
formas variantes con filas de la forma dominante, conserva además
`n_filas_formas_variantes` y `n_filas_formas_dominantes` con el reparto
completo, y `mostrados_formas_variantes` y `mostrados_formas_dominantes`
con el reparto de lo que sobrevivió al truncado. Las variantes se
entregan primero, de modo que el truncado no se lleve lo accionable.
Para `patron_raro`, el alcance puede ser `completo`, `muestra_patrones`,
`patrones_parciales` o `muestra_patrones+patrones_parciales`. El resumen
y el texto de evidencia de `patron_raro` muestran como maximo seis
patrones, pero la trazabilidad conserva los nombres de todos los
patrones raros hasta un limite de 5.000; `patrones_parciales` indica que
se alcanzo ese limite, no que se haya alcanzado el tope de presentacion.
Cuando se emite un hallazgo `patron_raro`, su evidencia incluye la
proporcion del patron dominante y cuantas filas pertenecen a patrones no
dominantes que superan `umbral_patron_raro` y por eso quedan excluidos.
Si el patron dominante no alcanza `umbral_patron_dominante`, no se emite
el hallazgo: `cobertura_diagnosticos` declara la no medicion, su
proporcion observada y el argumento que se puede ajustar. Si el conteo y
la traza no coinciden, conserva el hallazgo y emite una advertencia de
clase `lupa_trazabilidad_incoherente`. La guarda compara el total previo
al truncado y respeta la unidad declarada. Una columna compuesta no
analizada conserva en la traza todas sus filas. Si una columna de listas
se reconoce como constante pero no se puede contar su frecuencia, el
conteo afectado queda en NA y `cobertura_diagnosticos` explica la no
evaluación. Los índices no contienen valores. Usarlos para extraer filas
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
de `hallazgos`, con una fila por diagnóstico que no pudo evaluarse o
cuya enumeración quedó parcial y las columnas `diagnostico`, `columna`,
`motivo`, `como_resolverlo` y `dependencia`. Incluye la falta de
`stringdist`, `stringi`, `bit64` o `sf`, incluida la medición de
secuencias `integer64` cuando falta `bit64`, y las zonas horarias POSIXt
sin declarar. Los patrones de frecuencia intermedia no se consideran
desvios del patron dominante: `patron_raro` es completo respecto de su
criterio de rareza cuando no hay recorte de trazabilidad. Si el conjunto
de nombres raros supera 5.000, `cobertura_diagnosticos` declara el
recorte y su limite. Quien decida automáticamente sobre un perfil debe
revisar `nrow(perfil$cobertura_diagnosticos)` además de las severidades:
un perfil sin hallazgos y con diagnósticos no evaluados no es un perfil
limpio. Cuando una clave declarada no queda plenamente verificada,
`meta$clave` conserva los estados de unicidad y ausencia de nulos, sus
conteos y la semántica usada por la trazabilidad. Los tres responden
preguntas distintas y no comparten universo:

- `unicidad` se evalúa **sólo entre las filas con la clave completa**
  (`semantica = "claves_completas"`), porque una repetición entre filas
  incompletas no viola la unicidad: en SQL dos `NULL` no son iguales.
  `filas_evaluadas` cuenta esas filas y `filas_totales` la tabla entera.
  Su estado es `"verificada"`, `"refutada"`, `"no_verificada"` cuando no
  se pudo comparar, o `"sin_casos_evaluables"` cuando **ninguna** fila
  tiene la clave completa: ahí la unicidad sería cierta sobre un
  conjunto vacío, que es cierto y engañoso a la vez, y por eso tiene
  estado propio.

- `ausencia_nulos` responde si todos los componentes están presentes. Su
  hallazgo asociado, `clave_con_ausentes`, cuenta las filas con al menos
  un componente ausente y conserva sus índices.

- `clave_no_unica` sólo se emite cuando hay valores repetidos entre las
  filas completas y usa `filas_evaluadas` como `n_evaluados`; no
  convierte una colisión entre ausentes en una violación de unicidad.

- `trazabilidad` conserva la semántica de R, que es la que localiza las
  filas, e informa en `colisiona_con_ausentes` si el localizador queda
  ambiguo porque dos filas con ausentes comparten representación.

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
explícitamente la lista de naniar. La política se normaliza como un
conjunto numérico: el orden, el tipo entero o doble, los duplicados y
los `NA` no cambian su interpretación.

`muestra` limita el descubrimiento de patrones, la inferencia de tipos,
la detección de formatos de fecha y la búsqueda de dependencias
funcionales. Las demás métricas y hallazgos se calculan sobre todas las
filas. En los resúmenes que convierten texto con el tipo o formato
descubierto en esa muestra, los valores presentes que no se pueden
convertir no se ocultan: `n_fechas_excluidas_granularidad` los cuenta
para fechas y `n_valores_excluidos_resumen` para números, y el estado
deja de ser un cálculo completo. `n_valores_excluidos_resumen` cuenta
también los valores **no finitos** que el resumen deja afuera, y lo hace
en cualquier clase: una columna de fechas con un `Inf` publica
`n_valores_excluidos_resumen = 1`, `n_fechas_resumidas` sin contarlo y
el estado `"calculados_sobre_valores"`. Y para las longitudes de texto,
`n_longitudes_resumidas` declara sobre cuántos valores se calcularon
`longitud_minima`, `longitud_maxima` y `longitud_media`:
[`nchar()`](https://rdrr.io/r/base/nchar.html) no puede medir un valor
cuyos bytes no son UTF-8 válido, y ése queda fuera del promedio. La
causa viaja aparte, en `n_codificacion_invalida`. Por eso
`meta$filas_analizadas` describe el máximo usado por los análisis
muestreados, no el alcance del perfil completo. En cada fila de
`columnas`, `n_filas_analizadas_tipo` y `muestreado_tipo_inferido`
declaran el alcance concreto de `proporcion_tipo_inferido`; no debe
interpretarse esa proporción como si hubiera usado necesariamente toda
la columna.

El tipo se **infiere** de los valores sólo en las columnas de texto: en
las demás es el tipo con que R las guarda —lógico, entero, doble,
fecha—, que no depende de los valores. Por eso una columna no textual
sin ningún valor conserva su tipo de almacenamiento, con
`proporcion_tipo_inferido` en `NA` porque ningún valor lo midió, y una
columna de texto sin valores queda `"desconocido"`, porque ahí no hay
nada de qué inferirlo.

Cuando el tipo inferido es `"fecha"` o `"fecha-hora"`,
`estado_tipo_inferido` declara además cómo quedó establecida esa
lectura: `"confirmado"` si todo formato con casamientos es inequívoco, y
`"candidato"` si el veredicto se apoya en casamientos ambiguos — el caso
de una columna donde `proporcion_tipo_inferido` llega a 1 sin un solo
valor inequívoco. En los demás tipos la columna queda `NA`. El resumen
impreso anota `(candidato)` junto al tipo cuando corresponde; la
conversión, el rango temporal y la remediación ya exigían formatos
confirmados y no cambian.

En una columna temporal, `minimo`, `maximo`, `media` y `mediana` quedan
en `NA` y su valor viaja en `minimo_fecha`, `maximo_fecha`,
`media_fecha` y `mediana_fecha`, que son texto legible. `desvio` es la
excepción y conviene saberlo: no es un momento sino una duración, así
que se informa como número, y ese número está **en segundos** tanto para
`"fecha"` como para `"fecha-hora"`, porque las dos clases se unifican en
esa unidad antes de resumirlas. Un desvío de `136610.4` sobre una
columna de fechas son 1,6 días.

Ese detalle importa al comparar las dos puertas.
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
no clasifica tipos: informa el que declara el motor, y un motor que no
preserva `DATE` ni `BOOLEAN` —SQLite guarda ambos como número— hace que
esas columnas se midan como números. La misma columna de fechas da
entonces `desvio` en días por la puerta DBI y en segundos por ésta, y su
`moda` sale como el entero crudo del motor en vez de la fecha
formateada. No es una discrepancia de cálculo: cada puerta describe lo
que tiene delante, y lo que tiene delante es distinto.

Una columna cuyo año se expresa con dos dígitos se informa con su
`tipo_inferido` —`"fecha"` o `"fecha-hora"`— pero deja `minimo_fecha`,
`maximo_fecha`, `media_fecha` y `mediana_fecha` en `NA`. No es una
omisión: `23` puede ser 1923 o 2023, y elegir el siglo para calcular un
rango sería inventarlo. El hallazgo `anio_de_dos_digitos` señala esas
columnas, y el rango aparece una vez que el usuario resuelve la
ambigüedad. Lo mismo vale para un valor que dos formatos confirmados de
la misma columna leen distinto: en una columna que tiene `13/06/2020` y
`06/30/2020`, el valor `01/12/2020` puede ser el 1 de diciembre o el 12
de enero, y no se decide por la mayoría. Queda fuera del rango, y
[`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
bloquea la conversión de la columna mientras queden valores así. El
resumen de fechas usa la misma regla de validez que la detección: un
`99991231` que el formato `%Y%m%d` no admite tampoco llega a
`maximo_fecha`. Cuando no sobrevive ningún valor utilizable, el estado
distingue **dos afirmaciones que no son la misma**: `"sin_valores"` dice
que la columna no tenía ningún valor presente **en el universo que el
resumen mide** —vacía, todo `NA`, o con la aplicabilidad declarada
dejando ese universo sin valores— y `"sin_valores_utilizables"` dice que
sí los tenía y ninguno servía para el resumen. La relación que se cumple
con la fila es `n_aplicables - n_faltantes`, no `n - n_faltantes`: con
aplicabilidad declarada los valores de afuera del universo se declaran
en `n_presentes_fuera_de_aplicabilidad` y no cuentan para este estado.
Una columna de treinta `Inf` es el segundo caso y no el primero: publica
`n_faltantes = 0` y `n_distintos = 1`, así que decir «sin valores» de
ella contradiría a su propia fila. La otra puerta del segundo caso son
los centinelas que se llevan todos los valores. La presencia se define
igual que en `n_faltantes`, que cuenta el `NaN` como ausente: una
columna de treinta `NaN` es `"sin_valores"`.

**La `media` se acumula por bloques y puede diferir de
[`mean()`](https://rdrr.io/r/base/mean.html) en los últimos bits; la
`mediana` es exacta.** El paquete no promete exactitud bit a bit para la
media —sí para la mediana, que se calcula con
[`median()`](https://rdrr.io/r/stats/median.html) sobre el vector entero
y por eso no se reemplazó por el cuantil 0,5—. Medido: sobre veinte mil
valores de `1e12` y veinte mil de `1e-6` la diferencia es de `6,1e-05`,
y sobre treinta mil valores con signo del orden de `1e9` es de
`2,3e-10`; con valores de magnitud parecida no hay diferencia en ningún
bit. Quien rehaga [`mean()`](https://rdrr.io/r/base/mean.html) sobre una
columna grande y vea otro último dígito está viendo esto y no un error.

## La unidad viaja al lado de la cifra

El campo `unidad` dice **en qué unidad están las cifras que la fila
publica** —`minimo`, `maximo`, `media`, `mediana`, `desvio`—, y queda en
`NA` cuando la columna no declara ninguna. Es una sola pregunta, y por
eso no se confunde con `numero_texto_unidad`, que es la unidad que un
valor de texto traía pegada al número (`"12 kg"`), ni con la `unidad` de
[`clasificar_variables()`](https://sebollin.github.io/lupa/reference/clasificar_variables.md),
que es la unidad **declarada** del dato.

Sin este campo, cuatro columnas medidas en unidades distintas publicaban
filas idénticas: `units::set_units(c(1, 2, 3), "m")` y la misma en
`"km"` dan las dos `media = 2` y `desvio = 1`, y un `Period` de dos días
publicaba `172800` sin decir que eran segundos. Por clase:

- `units` y `difftime`: la unidad declarada, en forma canónica (`km/h`,
  `m^2`, `kg*m/s^2`). `difftime` **publica sus estadísticos en esa
  unidad**: una columna de minutos publica `media = 75` con
  `unidad = "mins"`. Se abstuvo mientras no existía dónde publicar la
  unidad —un `75` sin decir de qué no es una medida—, y era el único que
  se abstenia: `Date` y `POSIXt` publican `desvio` en segundos desde
  antes. Los valores van en la unidad declarada por la columna, sin
  convertir a segundos: convertirlos publicaría un número que no está en
  la columna.

- `Period`, `Duration`, `Date` y `POSIXt`: `"segundos"`, que es la
  unidad en la que sale `desvio` y que antes sólo estaba dicha en prosa.

- cualquier otra columna que **declare** un atributo `units` —eso
  circula como metadato en datos importados— publica lo declarado,
  porque sus cifras están en esa unidad.

- el resto: `NA`. El campo no inventa una unidad donde no hay ninguna.

**Y se calla cuando la fila no publica ninguna cifra cuantitativa**, o
sea cuando `estado_resumen_cuantitativo` vale `"no_aplica"`: el atributo
`units` lo puede llevar cualquier clase, y un `factor` con
`attr(x, "units") <- "kg"` publicaba `unidad = "kg"` al lado de
`media = NA`. Una unidad de cifras que no existen no contesta la
pregunta del campo. El discriminador es ese estado y no una lista de
clases, así que una columna `units` de puros `NA` **sí** declara su
unidad: su clase se resume, y la unidad dice en qué estarían sus cifras.

Si una columna declara **más de una** unidad —un atributo `units` de
largo mayor que uno, que ningún objeto `units` produce pero un dato
importado sí—, el campo las publica todas separadas por coma. Ese valor
no es una unidad: dice que la declaración está mal. Quedarse con la
primera sería elegir una por el usuario, que es el defecto que este
campo vino a cerrar.

Y
[`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md)
compara este campo como un aspecto propio, con la severidad de un cambio
de tipo: publicar la unidad sin compararla dejaba en pie el defecto
entero, porque el problema era que **una entrega donde cambió la unidad
no mostraba cambio**.

Una columna de períodos expresados sólo como mes y año informa la
`granularidad` `"mes"` en `formatos_fecha` y deja los resúmenes de fecha
en `NA` con `estado_resumen_cuantitativo = "granularidad_incompleta"`:
asignar el día 1 para obtener un mínimo o una media también sería
inventar un dato. Si esos períodos son minoritarios dentro de una
columna que también contiene fechas con día, los resúmenes se calculan
sólo sobre las fechas completas y declaran
`estado_resumen_cuantitativo = "calculados_sobre_dias"`, junto con
`n_fechas_resumidas` y `n_fechas_excluidas_granularidad`. El mínimo y el
máximo son entonces condicionales al subconjunto con día; no representan
un rango de toda la columna.

Los dos conteos que acompañan a ese estado cuentan cosas distintas.
`n_fechas_excluidas_granularidad` cuenta **sólo** las fechas que
quedaron fuera por ser de granularidad mes, y queda en `NA` cuando los
formatos se descubrieron sobre una muestra: ese número se deriva de los
formatos detectados en la muestra mientras el resumen recorre la columna
entera, así que atribuir la causa exigiría rehacer la detección sobre
todo, que es el costo que el muestreo existe para evitar.
`n_valores_excluidos_resumen` es el campo general —el mismo que usan las
columnas numéricas— y cuenta **todo** valor presente que no sostiene el
resumen, sea un período de mes o un texto que ningún formato pudo leer.
Siempre que ese total sea mayor que cero, el perfil agrega una fila
`resumen_cuantitativo` en `cobertura_diagnosticos`, haya muestreo o no.

Para números ordinarios, los estadísticos cuantitativos se calculan sólo
con valores finitos; `n_nan`, `n_infinito_positivo` y
`n_infinito_negativo` declaran lo excluido. En columnas `integer64` que
exceden el entero máximo representable exactamente por `double`,
`minimo` y `maximo` quedan en `NA` y los extremos exactos se conservan
en `minimo_exacto` y `maximo_exacto`. Cuando la conversión de texto deja
valores presentes afuera —haya muestreo o no, porque la conversión
descarta lo que no puede leer en los dos casos—,
`n_valores_excluidos_resumen` y
`estado_resumen_cuantitativo = "calculados_sobre_valores"` declaran ese
alcance parcial.

Los limites de Tukey se publican solo cuando hay al menos 20 valores
finitos y el recorrido intercuartilico es positivo. Si falta alguna
condicion, `n_outliers` queda en `NA` y `cobertura_diagnosticos` explica
el motivo y como reunir evidencia suficiente; un IQR cero no convierte
una categoria minoritaria en outlier.

El paquete distingue lo que el usuario **declara** de lo que él mismo
**sospecha**, y esa distinción gobierna los resúmenes. Cuando se
reemplaza `sentinelas_numericos`, esos valores son una declaración de
ausencia y salen de `media`, `mediana`, `minimo`, `maximo`, `desvio` y
`n_outliers`, igual que salen los `NA` y las filas que `aplicabilidad`
deja fuera del universo; `n_valores_excluidos_resumen` y
`estado_resumen_cuantitativo` declaran ese alcance. La lista por omisión
—`-9`, `-99`, `-999`, `-9999` y `999`— es en cambio una conjetura del
paquete: se informa como `faltantes_disfrazados` pero **no** se retira
de los resúmenes, porque `999` también puede ser un dato legítimo y
actuar sobre una sospecha cambiaría números que nadie pidió cambiar.

`moda`, `frecuencia_moda`, `n_distintos`, `tasa_distintos` y
`longitud_*` describen la **representación almacenada** y siguen
contando esos valores, del mismo modo que cuentan un `Inf` que los
estadísticos excluyen o un `"N/A"` textual. Por eso `moda` puede quedar
fuera del rango que informan `minimo` y `maximo`: no es una
contradicción sino la diferencia entre describir lo guardado y resumir
lo que vale como dato, y los conteos —`n_valores_excluidos_resumen`,
`n_infinito_positivo`, `n_faltantes_disfrazados`— dicen cuánto separa a
los dos. Una columna de listas intenta contar sus valores distintos; si
la clase no admite comparación, informa `NA` en lugar de afirmar cero.
Las columnas compuestas —matrices o arreglos de más de una dimensión— se
conservan como una unidad por fila: `n` informa las filas de la tabla,
pero los estadísticos por valor quedan en `NA` y un hallazgo explica que
deben separarse en columnas con semántica explícita. Cuando todos los
valores válidos aparecen una sola vez **no hay moda**, y `moda` queda en
`NA`. Lo que se publicaría es el ganador de un desempate de tantas vías
como valores haya, y ese desempate sigue el orden de ordenamiento, que
depende de cómo esté guardada la columna: la misma columna
`c(-5.5, -1, 0, 3.75)` daba `-5.5` como número y `-1` como texto.
`frecuencia_moda` se conserva —vale 1, es cierto y es la evidencia de
por qué la moda quedó callada—. Con un solo valor distinto sí hay moda,
aunque su frecuencia sea 1: ahí no hay empate que resolver.

Una columna numérica emite `valor_concentrado` como señal `sospechoso`
cuando tiene al menos 20 valores válidos y 10 valores distintos, y su
moda tiene una frecuencia al menos cinco veces mayor que la del segundo
valor más frecuente y representa al menos 0,15 de los valores válidos.
La elegibilidad es parte de la señal: una columna con menos categorías
no recibe una fila de `cobertura_diagnosticos`, porque allí la moda es
la distribución. La evidencia publica el valor modal, ambas frecuencias,
el cociente y la fracción. M2 fue la regla seleccionada en
`.trabajo-agente/medicion-concentracion.md`: produjo cero falsos
positivos en 114 columnas limpias, con un margen medido de 2,2 veces. Es
una sospecha, no una acusación: un valor legítimo puede dominar. Sus
puntos ciegos medidos son las concentraciones por debajo de 15 % y los
empates naturales en columnas enteras pequeñas, donde el cociente puede
quedar por debajo de cinco.

La ley de Benford se evalúa sólo en columnas numéricas con al menos 50
valores finitos; las columnas compuestas —matrices o arreglos de más de
una dimensión— no son magnitudes por fila y quedan fuera del análisis.
Antes de comparar exige variación, que la columna no parezca un
identificador ni una secuencia correlativa, al menos 100 observaciones
positivas utilizables, una proporción de positivos igual a 1 y tres
órdenes de magnitud según `log10(max/min)`. Si falla alguna precondición
no emite un hallazgo: la enumera en `cobertura_diagnosticos`. Si aplica,
`meta$benford$resultados` conserva la distribución observada y esperada
por primer dígito, el chi-cuadrado de Pearson, ocho grados de libertad y
el valor p; `meta$benford$umbrales` publica todos los cortes. Un valor p
menor que `0.01` genera `desviacion_benford` como señal descriptiva para
revisar, no como evidencia de fraude o manipulación. Topes
administrativos, redondeos, precios psicológicos y subsidios de monto
fijo son explicaciones posibles.

Las relaciones aritméticas se buscan sólo entre columnas numéricas **sin
clase declarada** y con variación: una columna que declara una clase
propia —`Date`, `POSIXt`, `difftime`, `integer64`, `units`,
`lubridate::Period` o cualquier otra— no participa, porque sumarla o
dividirla no es la aritmética de un doble; el texto numérico y las
columnas constantes tampoco. Las Las columnas que **publican cifras
cuantitativas** y quedan fuera por su clase se declaran una por una en
`cobertura_diagnosticos`: `integer64`, `units`, `lubridate::Period`,
`haven_labelled` y `difftime`. El criterio **no** es
[`is.numeric()`](https://rdrr.io/r/base/numeric.html) —que responde
`FALSE` sobre un `difftime`— sino si la fila publica `minimo`, `maximo`
y `media` como números: ahí la ausencia de hallazgo se lee como
conformidad y por eso hay que declararla. `difftime` entró en esa
categoría cuando dejó de abstenerse del resumen cuantitativo, y sin
declararlo quedaba mudo en los dos diagnósticos mientras `units` los
declaraba —la misma columna, con las mismas cifras, tratada de dos
maneras—.

Una **fecha** y una **hora** no entran en esa declaración, y eso es
deliberado: no publican `media` como número pelado —su resumen sale en
`media_fecha` y compañía—, así que su silencio se lee distinto, y no son
magnitudes para estos diagnósticos. Quedan fuera del mismo modo que una
columna de texto, sin fila propia. `meta$aritmetica_columnas` publica
las clases medidas en esta tabla. La ley de Benford sigue el mismo
criterio y la misma declaración. Cada relación requiere al menos tres
filas con valores finitos en todas las columnas involucradas; los `NA`,
`NaN` e infinitos quedan fuera del universo que publica la evidencia.
Para cada terna se prueban las tres orientaciones de una identidad
aditiva; esto cubre sumas y sus restas equivalentes sin informar tres
veces la misma igualdad. En pares proporcionales, `k` es la mediana de
los cocientes finitos cuya base no es cero, pero el cumplimiento se
evalúa después también en las filas con base cero. Si una identidad
aditiva ya relaciona una terna, se omiten las proporcionalidades
redundantes entre su total y sus sumandos; se conserva la
proporcionalidad entre los dos sumandos. Una regularidad completa se
informa con severidad `"ok"`; si alcanza el umbral pero tiene
discrepancias, sigue el criterio de las relaciones de orden y es
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

Un valor `double` **subnormal** —distinto de cero y menor que
`.Machine$double.xmin` en valor absoluto, unos 2,2e-308— casi siempre
sale de reinterpretar un patrón de bits o de un desbordamiento por
defecto. El hallazgo `valores_subnormales` los cuenta y da sus filas,
con severidad `sospechoso`. El caso que motiva el diagnóstico es leer
una tabla donde una columna de enteros grandes se escribió como doble
reinterpretando los bits: los números salen del orden de 1e-314 y, al
venir así desde el origen, ninguna comprobación cruzada los contradice.

**No es concluyente, y el hallazgo no lo afirma.** Un cálculo que
desborda por defecto cae legítimamente en ese rango —`2^-1050` da
8,3e-317—, y una columna de p-valores o de verosimilitudes muy pequeñas
puede dispararlo sin que haya nada roto. Lo que se publica es el hecho
medido —cuántos valores son subnormales y en qué filas—, no un veredicto
sobre su origen.

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

Hay un segundo caso en que `unicode_evaluado` queda en `FALSE`, y no es
una dependencia ausente: cuando **todos** los valores de la columna
traen bytes que no forman UTF-8 válido, no queda texto que analizar. Ahí
las pasadas que dependen de texto legible no llegan a correr, así que
`n_codificacion_rota`, `n_codificacion_reparable`,
`n_codificacion_reparable_parcialmente`, `n_codificacion_irreparable`,
`n_codificacion_no_se_pudo` y `estado_codificacion_reparacion` quedan en
`NA` en vez de en cero: un cero afirmaría no haber encontrado nada, y lo
cierto es que no hubo nada que mirar. `n_codificacion_invalida` sigue
contando esos valores y el hallazgo `codificacion_invalida`, de
severidad `error`, sigue declarando que se excluyeron.

Esas cifras **no son una partición**. `n_codificacion_rota` cuenta cada
valor afectado una sola vez; `n_codificacion_reparable` y
`n_codificacion_reparable_parcialmente`, los que el motor reparó del
todo o en parte; `n_codificacion_irreparable`, los que traen el carácter
de reemplazo `U+FFFD`, cuyo original ya no está; y
`n_codificacion_no_se_pudo`, los que el motor reconoce como mal
convertidos y no pudo reparar. Un valor con `U+FFFD` suele contar en las
dos últimas, así que sumarlas no da `n_codificacion_rota`.

Los **nombres de columna** que el perfil publica —en `columnas$columna`
y en todo lo que nombre una columna: claves, relaciones, dependencias,
hallazgos, patrones, el plan de remediación y el reporte— son los
nombres de la tabla de entrada tal como llegaron, con sus bytes **y su
marca de codificación**. Eso significa que sirven para indexar esa misma
tabla —`datos[[nombre]]`— sea cual sea el `LC_CTYPE` de la sesión. El
paquete deriva una representación de trabajo para comparar y desambiguar
internamente, pero no la publica: dos nombres con bytes distintos siguen
siendo dos columnas distintas. Al componer texto publicado —en
particular las sugerencias de `posible_ausencia_estructural`— usa una
copia marcada como UTF-8 del nombre, sin modificar el nombre conservado
en la tabla. Por eso una sugerencia como
`perfilar(datos, aplicabilidad = list(entero = ~ categoría == "categoría"))`
conserva el nombre real también bajo `LC_CTYPE = "C"` y se puede copiar
y pegar.

La identidad de la columna no depende de esa exclusión: `n_distintos`,
`moda` y `frecuencia_moda` comparan la representación almacenada, que se
puede distinguir sin decodificarla. Un valor ilegible cuenta como valor,
y la moda que se publica es el valor original tal como llegó, sin
reinterpretarlo. Cuando ese valor es numérico, su representación textual
se formatea con notación fija, con la precisión completa e independiente
de `scipen` y de `digits`; se conserva `OutDec`, porque esa marca
decimal sí pertenece a la preferencia de locale de quien usa el paquete.

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
ausente.

Una columna cuyos valores son `data.frame` o matriz —lo que devuelve un
driver al leer un `STRUCT`— declara `n` en **filas**, como cualquier
otra: tiene tantos valores como la tabla, y lo que no se puede es
analizarlos como texto, que se declara aparte en
`cobertura_diagnosticos`.

**Con un universo aplicable declarado**, los conteos y los índices de
geometría se restringen a ese universo, igual que el resto del
perfilado: una geometría que la regla dejó afuera no cuenta como vacía,
ni como inválida, ni como fuera de dominio, y `n_geometrias`,
`n_dominio_evaluados` y `n_validez_evaluados` describen el universo.
Tres campos son la excepción y conviene saber por qué:
`n_bbox_evaluados`, `n_geometrias_analizadas` y `n_vertices_analizados`
cuentan **el trabajo que se hizo**, no filas de un universo —el análisis
geométrico corre sobre la columna entera, porque necesita su CRS y sus
tipos—, así que se publican tal como se midieron. Recortarlos a ojo
sería inventar un número: no hay conjunto de índices del que derivarlos.

**Una columna de texto puede ser una geometría.** Se reconoce como WKT o
WKB (crudo o hexadecimal) cuando la mayoría de una muestra de 20 valores
tiene esa forma; si después hay valores que no se pueden convertir, la
pérdida se declara en `motivo_representacion`. Esa columna se sigue
perfilando como texto en lo que toca a su escritura —espacios,
codificación, invisibles—, pero no recibe los diagnósticos que leen los
valores como palabras o como claves —la proximidad de vocabulario,
`posible_identificador`, `patron_raro`—: `cobertura_diagnosticos`
declara que no aplican. El argumento `normalizar` declara el perfil de
comparación que se conserva en `meta$normalizacion`; cambia sólo la
representación usada para comparar, no el texto guardado. `TRUE` usa el
perfil predeterminado, `FALSE` desactiva sus pasos configurables,
`"amplio"` activa los tres pliegues optativos y
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
columnas sin ausentes que tienen al menos 100 filas y 90 % de valores
distintos producen un hallazgo `casi_clave` cuando el valor dominante
concentra al menos la mitad de los duplicados excedentes. La
concentración se calcula sobre las repeticiones posteriores a la primera
de cada valor, no sólo sobre la tasa de distintos. La evidencia declara
el mínimo de filas, ambos umbrales, los valores que colisionan y sus
frecuencias. Las variables con rol propuesto `fecha`, incluidas
fecha-hora, se excluyen. Así una colisión concentrada queda separada del
texto libre de alta cardinalidad con repeticiones dispersas. Un vector
`double` sólo participa si todos sus valores finitos son enteros; así se
conservan identificadores importados con ese almacenamiento y se
excluyen medidas con alguna parte fraccionaria, como importes o
coordenadas. Los vectores `integer64` se tratan como enteros semánticos.
La evidencia del hallazgo declara este criterio y el recuento observado.
Además, si `casi_duplicados_vocabulario = TRUE`, el perfil busca
variantes casi duplicadas en columnas de texto. Agrupa el vocabulario
crudo mediante fusiones exactas de la normalización y estrellas de
distancia centradas en un valor de frecuencia estrictamente mayor y
único; los empates no se fuerzan. No cierra cadenas transitivamente ni
elige una forma canónica. La unidad es el valor distinto, no la fila, y
cada variante conserva su frecuencia. Cada grupo declara sus distancias
mínima y máxima. El límite `max_proporcion_grupo_vocabulario` evita
presentar un grupo que abarque casi toda la columna como un diagnóstico
útil: en ese caso el alcance dice que el diagnóstico no aplica. Ese
límite se activa desde 20 valores distintos o cuando el grupo mayor ya
tiene 10 variantes; sólo suprime el grupo si además ocupa una fracción
mayor que el umbral. Así un grupo de tres en cuatro valores se entrega,
pero quince variantes que ocupan toda una columna no se presentan como
una sola familia. Un grupo grande dentro de un vocabulario mucho mayor
puede seguir pasando si su proporción es pequeña. El alcance expone
ambos cortes. El argumento permite apagar el detector cuando no
corresponde a la tabla. Si hay pares cercanos pero todas las frecuencias
empatan, el alcance declara que no hubo asimetría para formar una
estrella y sugiere
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

La corazonada de centinelas numéricos se apaga sobre una **numeración
limpia**, para no llamar ausencia a un código válido. `densa` responde
sólo a la cobertura del rango —`densidad_secuencia_entera` por encima de
su umbral—; `moda_sobresale_secuencia_entera` es una señal independiente
y no cambia los tres escudos de forma que dependen de la numeración. En
una columna `integer64`, `bit64` se carga de manera diferida si está
instalado para registrar sus métodos antes de medir. Si no está
instalado, las cinco medidas públicas de la secuencia quedan en `NA` y
`cobertura_diagnosticos` declara que `secuencia_entera` no se evaluó por
falta de esa dependencia. Lo mismo pasa si la columna tiene valores por
encima de 2^53: en un doble se redondean, la densidad no se mide sobre
números redondeados, y la secuencia queda en `NA` y declarada, no como
«no densa». La guarda vuelve a abrirse si un candidato presente queda
fuera del rango de los valores restantes de la numeración, o si la
frecuencia del candidato es la moda sobresaliente. Esta segunda señal se
mide por el **salto entre frecuencias consecutivas**, ordenadas de mayor
a menor y mirando sólo las primeras posiciones. Es el mismo idioma que
`salto_de_escala_secuencia_entera` usa con los huecos: lo que delata a
un centinela no es que su frecuencia sea grande, sino que haya un
**acantilado** entre el grupo que sobresale y el resto de la
distribución.

Comparar contra un valor concreto no alcanza, y cada intento lo rompió
un caso distinto: contra el **segundo** valor, un señuelo legítimo y
frecuente infla el segundo puesto; contra la frecuencia **típica**
exigiendo forma de numeración, un centinela masivo sobre una clave
foránea nunca la tiene; y con las dos juntas, **varios centinelas
empatados se cubren entre sí** —tres valores con la misma frecuencia
dejan «máximo igual al segundo» y la comparación vacía—. El acantilado
ve los tres porque no le importa quién es el segundo sino dónde se corta
la distribución.

Se miran sólo las primeras posiciones porque el grupo que sobresale está
arriba: en la cola, una caída de dos apariciones a una da un salto que
no dice nada de la columna. Medido sobre las diez columnas numéricas del
banco real y dos formas de clave foránea, ninguna pasa de 1,71; los
casos que hay que atrapar van de 4,75 a 249. Perder la condición de
numeración no genera un hallazgo por sí solo: sólo devuelve la columna a
la mirada de la lista de centinelas. En una numeración compacta, un
candidato fuera de rango también abre la guarda aunque su frecuencia no
forme un acantilado.

El factor de frecuencia sólo califica la señal `moda_sobresale`: una
caída menor no abre esa vía, pero tampoco bloquea la vía independiente
del rango. Así, un candidato fuera de rango se informa aunque aparezca
cuatro veces y no forme un acantilado. Lo que el usuario declara con
`sentinelas_numericos` atraviesa la guarda y se informa con severidad
`error`, que es la regla general del paquete: excluye lo que se declara
e incluye lo que sospecha.

La clasificación de posibles datos personales es más amplia que la
protección. Cada clasificación declara `poder_discriminante` y
`proteger`:

- `debil`: una forma genérica, como siete a doce dígitos, coincide
  también con importes, facturas y códigos; se informa pero no se
  ocultan valores;

- `medio`: el nombre de la columna expresa una categoría personal (por
  ejemplo `telefono`, `fecha_nacimiento` o `fecha_fallecimiento`); se
  protege aunque sus valores no se puedan validar. El nombre tiene
  prioridad sobre una forma numérica genérica y también determina la
  etiqueta de tipo. También es `medio` una columna que trae correos
  completos sin que sean la mayoría de sus valores: un correo es un
  correo aunque esté solo, y la columna se protege;

- `alto`: una forma muy específica, como un correo, o nombre y forma se
  apoyan mutuamente; se protege;

- `verificado`: al menos tres valores distintos y al menos el 90% cumple
  uno de los validadores personales configurados; se protege incluso sin
  un nombre orientador. La proporción medida —la que se compara contra
  ese umbral— se publica en `proporcion_verificada`, para que una
  columna con el 90% no se lea igual que una con el 100%. El pack
  uruguayo es el predeterminado, pero puede reemplazarse por un
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
y extremos o medianas que corresponden a observaciones reales. Los
desvíos se conservan; las medias se protegen en sus dos formas (`media`
y `media_fecha`), porque la media de una columna personal puede
reconstruir demasiado. Los desvíos siguen siendo síntesis no ligadas a
una fila; `detalle_proteccion_personal` hace visible la supresión. En
fechas de nacimiento y fallecimiento, un hallazgo separado conserva el
diagnóstico de valores anteriores a 1900 o posteriores a la corrida sin
publicar las fechas. Los números escritos como texto reconocen tanto
coma como punto decimal y sus separadores de miles simétricos. Los
prefijos de tres letras separados del número, incluso como sufijo, `U$S`
y los símbolos monetarios se conservan como evidencia; `monedas_mixtas`
informa sus frecuencias sin convertir ni suponer tasas de cambio. Una
única moneda o un símbolo `$` aislado no produce ese hallazgo. Un sufijo
de unidad se reconoce sólo si es `%` o una abreviatura alfabética en
minúsculas; por eso `12 kg` y `13500 g` son unidades, mientras que `12A`
y `13B` se tratan como códigos. Si hay más de una unidad observada,
`unidades_mixtas` informa sus frecuencias y no convierte ni compara sus
magnitudes. Una única unidad no genera ese hallazgo.
`formatos_fecha_mixtos` compara representaciones dentro de una columna
cuyo dominio es la fecha o la fecha-hora. Una fecha agregada adentro de
una columna de horas del día —`12/02/2011 6:55 a.m.` entre valores
`7:10 a.m.`— no produce ese hallazgo, porque la columna no es de fechas:
aparece como `patron_raro`, que es lo que de verdad cambió. Decidir que
esa columna sólo debe contener horas es una regla de dominio y el
paquete no la supone. Del mismo modo, un número sin sufijo no cuenta
como una segunda unidad: una columna que mezcla `5` con `5 %` observa
una sola unidad y se informa como `numero_como_texto`, no como
`unidades_mixtas`. `celdas_multivaluadas` es deliberadamente
conservador: usa los patrones de
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
#> ! 9 hallazgos sospechosos
#> ✔ 8 hallazgos informativos ok
#> ℹ 8 diagnosticos no evaluados
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
#>           columna tipo_inferido prop_faltantes_totales n_distintos n_outliers
#>        id_persona         doble             0.00000000          11         NA
#>            cedula         texto             0.07692308          11         NA
#>  fecha_nacimiento         fecha             0.07692308          12         NA
#>              sexo         texto             0.15384615           4         NA
#>           ingreso         doble             0.07692308          12         NA
#>      departamento         texto             0.00000000          11         NA
#>              pais         texto             0.00000000           1         NA
#>            correo         texto             0.00000000          12         NA
#>          id_copia         doble             0.00000000          11         NA
#>        id_tramite identificador             0.00000000          12         NA
#> ℹ Se muestran 5 de 116 campos; los demás están en `columnas()`.
summary(perfil)
#>             columna tipo_declarado tipo_inferido estado_tipo_inferido
#> 1        id_persona          doble         doble                 <NA>
#> 2            cedula          texto         texto                 <NA>
#> 3  fecha_nacimiento          texto         fecha           confirmado
#> 4              sexo          texto         texto                 <NA>
#> 5           ingreso          doble         doble                 <NA>
#> 6      departamento          texto         texto                 <NA>
#> 7              pais          texto         texto                 <NA>
#> 8            correo          texto         texto                 <NA>
#> 9          id_copia          doble         doble                 <NA>
#> 10       id_tramite          texto identificador                 <NA>
#>    proporcion_tipo_inferido n_filas_analizadas_tipo muestreado_tipo_inferido  n
#> 1                 1.0000000                      13                    FALSE 13
#> 2                 1.0000000                      13                    FALSE 13
#> 3                 0.8461538                      13                    FALSE 13
#> 4                 1.0000000                      12                    FALSE 13
#> 5                 1.0000000                      13                    FALSE 13
#> 6                 1.0000000                      13                    FALSE 13
#> 7                 1.0000000                      13                    FALSE 13
#> 8                 1.0000000                      13                    FALSE 13
#> 9                 1.0000000                      13                    FALSE 13
#> 10                1.0000000                      13                    FALSE 13
#>    n_aplicables n_no_aplica n_aplicabilidad_indeterminada
#> 1            13           0                             0
#> 2            13           0                             0
#> 3            13           0                             0
#> 4            13           0                             0
#> 5            13           0                             0
#> 6            13           0                             0
#> 7            13           0                             0
#> 8            13           0                             0
#> 9            13           0                             0
#> 10           13           0                             0
#>    n_presentes_en_aplicabilidad_indeterminada
#> 1                                           0
#> 2                                           0
#> 3                                           0
#> 4                                           0
#> 5                                           0
#> 6                                           0
#> 7                                           0
#> 8                                           0
#> 9                                           0
#> 10                                          0
#>    n_presentes_fuera_de_aplicabilidad n_faltantes prop_faltantes
#> 1                                   0           0              0
#> 2                                   0           0              0
#> 3                                   0           0              0
#> 4                                   0           0              0
#> 5                                   0           0              0
#> 6                                   0           0              0
#> 7                                   0           0              0
#> 8                                   0           0              0
#> 9                                   0           0              0
#> 10                                  0           0              0
#>    n_faltantes_disfrazados n_faltantes_disfrazados_textuales
#> 1                        0                                 0
#> 2                        1                                 1
#> 3                        1                                 1
#> 4                        2                                 2
#> 5                        1                                 0
#> 6                        0                                 0
#> 7                        0                                 0
#> 8                        0                                 0
#> 9                        0                                 0
#> 10                       0                                 0
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
#> 5                   FALSE              1.199988e-06
#> 6                   FALSE                        NA
#> 7                   FALSE                        NA
#> 8                   FALSE                        NA
#> 9                   FALSE              1.000000e+00
#> 10                  FALSE                        NA
#>    n_posiciones_secuencia_entera n_huecos_secuencia_entera
#> 1                             NA                        NA
#> 2                             NA                        NA
#> 3                             NA                        NA
#> 4                             NA                        NA
#> 5                       10000099                  10000087
#> 6                             NA                        NA
#> 7                             NA                        NA
#> 8                             NA                        NA
#> 9                             11                         0
#> 10                            NA                        NA
#>    hueco_maximo_secuencia_entera salto_de_escala_secuencia_entera
#> 1                             NA                            FALSE
#> 2                             NA                            FALSE
#> 3                             NA                            FALSE
#> 4                             NA                            FALSE
#> 5                        9965499                             TRUE
#> 6                             NA                            FALSE
#> 7                             NA                            FALSE
#> 8                             NA                            FALSE
#> 9                              1                            FALSE
#> 10                            NA                            FALSE
#>    moda_sobresale_secuencia_entera umbral_densidad_secuencia_entera
#> 1                            FALSE                              0.8
#> 2                            FALSE                              0.8
#> 3                            FALSE                              0.8
#> 4                            FALSE                              0.8
#> 5                            FALSE                              0.8
#> 6                            FALSE                              0.8
#> 7                            FALSE                              0.8
#> 8                            FALSE                              0.8
#> 9                            FALSE                              0.8
#> 10                           FALSE                              0.8
#>    min_distintos_secuencia_entera              moda frecuencia_moda
#> 1                              20 [valor protegido]               2
#> 2                              20 [valor protegido]               2
#> 3                              20 [valor protegido]               2
#> 4                              20                 F               6
#> 5                              20             25000               2
#> 6                              20           Florida               2
#> 7                              20                UY              13
#> 8                              20 [valor protegido]               2
#> 9                              20                 1               2
#> 10                             20             TR001               2
#>    longitud_minima longitud_maxima longitud_media n_longitudes_resumidas minimo
#> 1               NA              NA             NA                     NA     NA
#> 2                3              11       9.692308                     13     NA
#> 3                4              10       9.384615                     13     NA
#> 4                0               3       1.076923                     13     NA
#> 5               NA              NA             NA                     NA    -99
#> 6                5              10       7.461538                     13     NA
#> 7                2               2       2.000000                     13     NA
#> 8               10              26      23.923077                     13     NA
#> 9               NA              NA             NA                     NA      1
#> 10               5               5       5.000000                     13     NA
#>     maximo        media mediana       desvio minimo_exacto maximo_exacto
#> 1       NA           NA      NA 3.546396e+00          <NA>          <NA>
#> 2       NA           NA      NA           NA          <NA>          <NA>
#> 3       NA           NA      NA 1.191710e+08          <NA>          <NA>
#> 4       NA           NA      NA           NA          <NA>          <NA>
#> 5  9999999 7.900192e+05   29900 2.767287e+06          <NA>          <NA>
#> 6       NA           NA      NA           NA          <NA>          <NA>
#> 7       NA           NA      NA           NA          <NA>          <NA>
#> 8       NA           NA      NA           NA          <NA>          <NA>
#> 9       11 5.923077e+00       6 3.546396e+00          <NA>          <NA>
#> 10      NA           NA      NA           NA          <NA>          <NA>
#>         minimo_fecha      maximo_fecha       media_fecha     mediana_fecha
#> 1               <NA>              <NA>              <NA>              <NA>
#> 2               <NA>              <NA>              <NA>              <NA>
#> 3  [valor protegido] [valor protegido] [valor protegido] [valor protegido]
#> 4               <NA>              <NA>              <NA>              <NA>
#> 5               <NA>              <NA>              <NA>              <NA>
#> 6               <NA>              <NA>              <NA>              <NA>
#> 7               <NA>              <NA>              <NA>              <NA>
#> 8               <NA>              <NA>              <NA>              <NA>
#> 9               <NA>              <NA>              <NA>              <NA>
#> 10              <NA>              <NA>              <NA>              <NA>
#>    n_fechas_resumidas n_fechas_excluidas_granularidad
#> 1                  NA                              NA
#> 2                  NA                              NA
#> 3                  11                               0
#> 4                  NA                              NA
#> 5                  NA                              NA
#> 6                  NA                              NA
#> 7                  NA                              NA
#> 8                  NA                              NA
#> 9                  NA                              NA
#> 10                 NA                              NA
#>    n_valores_excluidos_resumen n_ceros n_negativos n_outliers centinela_valor
#> 1                            0       0           0         NA              NA
#> 2                            0      NA          NA         NA              NA
#> 3                            2       0           0         NA              NA
#> 4                            0      NA          NA         NA              NA
#> 5                            0       1           2         NA              NA
#> 6                            0      NA          NA         NA              NA
#> 7                            0      NA          NA         NA              NA
#> 8                            0      NA          NA         NA              NA
#> 9                            0       0           0         NA              NA
#> 10                           0      NA          NA         NA              NA
#>    centinela_repeticiones densidad_sin_centinela n_nan n_infinito_positivo
#> 1                      NA                     NA     0                   0
#> 2                      NA                     NA     0                   0
#> 3                      NA                     NA     0                   0
#> 4                      NA                     NA     0                   0
#> 5                      NA                     NA     0                   0
#> 6                      NA                     NA     0                   0
#> 7                      NA                     NA     0                   0
#> 8                      NA                     NA     0                   0
#> 9                      NA                     NA     0                   0
#> 10                     NA                     NA     0                   0
#>    n_infinito_negativo estado_resumen_cuantitativo unidad zona_horaria_origen
#> 1                    0                  calculados   <NA>                <NA>
#> 2                    0                   no_aplica   <NA>                <NA>
#> 3                    0    calculados_sobre_valores   <NA>                <NA>
#> 4                    0                   no_aplica   <NA>                <NA>
#> 5                    0                  calculados   <NA>                <NA>
#> 6                    0                   no_aplica   <NA>                <NA>
#> 7                    0                   no_aplica   <NA>                <NA>
#> 8                    0                   no_aplica   <NA>                <NA>
#> 9                    0                  calculados   <NA>                <NA>
#> 10                   0                   no_aplica   <NA>                <NA>
#>    n_filas_fecha_civil_distinta_utc fecha_civil_distinta_utc
#> 1                                NA                       NA
#> 2                                NA                       NA
#> 3                                NA                       NA
#> 4                                NA                       NA
#> 5                                NA                       NA
#> 6                                NA                       NA
#> 7                                NA                       NA
#> 8                                NA                       NA
#> 9                                NA                       NA
#> 10                               NA                       NA
#>    representacion_geometria motivo_representacion crs_declarado tipo_geometria
#> 1                      <NA>                  <NA>          <NA>           <NA>
#> 2                      <NA>                  <NA>          <NA>           <NA>
#> 3                      <NA>                  <NA>          <NA>           <NA>
#> 4                      <NA>                  <NA>          <NA>           <NA>
#> 5                      <NA>                  <NA>          <NA>           <NA>
#> 6                      <NA>                  <NA>          <NA>           <NA>
#> 7                      <NA>                  <NA>          <NA>           <NA>
#> 8                      <NA>                  <NA>          <NA>           <NA>
#> 9                      <NA>                  <NA>          <NA>           <NA>
#> 10                     <NA>                  <NA>          <NA>           <NA>
#>    dimension_geometria dimensiones_no_evaluadas n_geometrias_vacias
#> 1                 <NA>                     <NA>                  NA
#> 2                 <NA>                     <NA>                  NA
#> 3                 <NA>                     <NA>                  NA
#> 4                 <NA>                     <NA>                  NA
#> 5                 <NA>                     <NA>                  NA
#> 6                 <NA>                     <NA>                  NA
#> 7                 <NA>                     <NA>                  NA
#> 8                 <NA>                     <NA>                  NA
#> 9                 <NA>                     <NA>                  NA
#> 10                <NA>                     <NA>                  NA
#>    n_geometrias_invalidas n_validez_evaluados validez_criterio
#> 1                      NA                  NA             <NA>
#> 2                      NA                  NA             <NA>
#> 3                      NA                  NA             <NA>
#> 4                      NA                  NA             <NA>
#> 5                      NA                  NA             <NA>
#> 6                      NA                  NA             <NA>
#> 7                      NA                  NA             <NA>
#> 8                      NA                  NA             <NA>
#> 9                      NA                  NA             <NA>
#> 10                     NA                  NA             <NA>
#>    validez_preprocesamiento n_fuera_de_dominio n_dominio_evaluados
#> 1                      <NA>                 NA                  NA
#> 2                      <NA>                 NA                  NA
#> 3                      <NA>                 NA                  NA
#> 4                      <NA>                 NA                  NA
#> 5                      <NA>                 NA                  NA
#> 6                      <NA>                 NA                  NA
#> 7                      <NA>                 NA                  NA
#> 8                      <NA>                 NA                  NA
#> 9                      <NA>                 NA                  NA
#> 10                     <NA>                 NA                  NA
#>    n_bbox_evaluados bbox_alcance bbox_xmin bbox_xmax bbox_ymin bbox_ymax
#> 1                NA         <NA>        NA        NA        NA        NA
#> 2                NA         <NA>        NA        NA        NA        NA
#> 3                NA         <NA>        NA        NA        NA        NA
#> 4                NA         <NA>        NA        NA        NA        NA
#> 5                NA         <NA>        NA        NA        NA        NA
#> 6                NA         <NA>        NA        NA        NA        NA
#> 7                NA         <NA>        NA        NA        NA        NA
#> 8                NA         <NA>        NA        NA        NA        NA
#> 9                NA         <NA>        NA        NA        NA        NA
#> 10               NA         <NA>        NA        NA        NA        NA
#>                      detalle_proteccion_personal n_blancos n_espacios_borde
#> 1  [estadisticos de orden y la media protegidos]         0                0
#> 2             [estadisticos de orden protegidos]         0                0
#> 3  [estadisticos de orden y la media protegidos]         0                0
#> 4                                           <NA>         1                0
#> 5                                           <NA>         0                0
#> 6                                           <NA>         0                0
#> 7                                           <NA>         0                0
#> 8             [estadisticos de orden protegidos]         0                0
#> 9                                           <NA>         0                0
#> 10                                          <NA>         0                0
#>    n_variantes_mayusculas n_variantes_unicode unicode_evaluado
#> 1                       0                  NA               NA
#> 2                       0                   0             TRUE
#> 3                       0                   0             TRUE
#> 4                       0                   0             TRUE
#> 5                       0                  NA               NA
#> 6                       0                   0             TRUE
#> 7                       0                   0             TRUE
#> 8                       0                   0             TRUE
#> 9                       0                  NA               NA
#> 10                      0                   0             TRUE
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
#> 1                                                               TRUE
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
#> 1               nombre                       NA
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
#> 1                              medio                    TRUE
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
