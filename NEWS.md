# lupa 0.1.0

## Patrones raros: ventana de operacion visible

- `patron_raro` declara en `cobertura_diagnosticos` cuando no puede ejecutarse
  porque el patron dominante no alcanza `umbral_patron_dominante`. La fila
  conserva la proporcion observada y explica como ajustar ese argumento.
- La evidencia de cada hallazgo `patron_raro` publica la proporcion del patron
  dominante y cuantas filas quedaron en patrones no dominantes excluidos por
  superar `umbral_patron_raro`. Ese conteo queda en la evidencia, no en la
  cobertura, porque no es una no medicion del diagnostico.

## Patrones raros y trazas accionables

- `patron_raro` conserva separado el tope de presentación y el alcance de la
  trazabilidad: `resumen_patrones` y la evidencia siguen mostrando como máximo
  seis patrones, mientras la traza usa sus nombres raros completos hasta un
  límite de 5.000. Si se alcanza ese límite, `cobertura_diagnosticos` y el
  alcance de la traza lo declaran.
- Las ausencias de una columna de lista se nombran, no sólo se cuentan. Una
  columna de listas —o un BLOB leído por `perfilar_dbi()`— informaba cuántos
  valores faltaban sin decir en qué filas, aunque `is.na()` los identifica
  elemento a elemento y es el mismo criterio con el que se contaron. Lo
  encontró la propia guarda de coherencia, que era exactamente para lo que se
  agregó.
- `casi_duplicados_vocabulario` entrega primero las filas de formas no
  dominantes y después las de formas dominantes. La unidad sigue siendo
  `valor_distinto`, el grupo sigue incluyendo la forma dominante y la evidencia
  informa cuántas filas mostradas pertenecen a cada tipo de forma.

## La traza de vocabulario y la guarda de coherencia cierran el circuito

- `casi_duplicados_vocabulario` conserva `unidad_conteo = "valor_distinto"`
  y ahora enumera las filas que contienen los valores de cada grupo
  seleccionado, incluida la forma dominante. La distancia sigue siendo una
  señal heurística, no una afirmación de identidad.
- `perfilar()` conserva cualquier hallazgo cuya traza no coincida y emite la
  advertencia de clase `lupa_trazabilidad_incoherente`. La guarda compara el
  total anterior al límite de presentación, funciona en ambas direcciones y
  adapta la comparación a la unidad declarada.

## Conteos y trazabilidad dejan de mezclar unidades

- `mayusculas_inconsistentes` y `normalizacion_unicode` declaran
  `unidad_conteo = "valor_distinto"` y cuentan en `n_evaluados` los valores
  distintos evaluados. `n_afectados` ya contaba esos valores; su traza sigue
  siendo por fila y enumera todas las filas que contienen los valores
  afectados, no sólo las defectuosas.
- `filas_duplicadas` cuenta ahora todas las filas participantes de los grupos,
  en línea con `EntidadDuplicada` y con `marcar_filas_duplicadas`. La evidencia
  conserva el número de excedentes para la acción que elimina repeticiones.
- Una constante de listas cuya frecuencia no puede contarse informa `NA` en
  `n_afectados` y deja el motivo en `cobertura_diagnosticos`; una matriz no
  analizada enumera todas sus filas en la trazabilidad.

## La trazabilidad deja de recalcular lo que el detector ya decidió

Un hallazgo dice cuántas unidades afecta y, cuando puede, cuáles. Ese «cuáles» lo
resolvía una rama de índices aparte que en varios casos aplicaba un criterio
**distinto** del detector que había producido el hallazgo. Los dos no coincidían, y
el desacuerdo no se veía porque nada comparaba la evidencia contra los índices.

- `patron_raro` no nombraba ninguna fila cuando la columna tenía algún patrón de
  frecuencia intermedia. La guarda comparaba el total de patrones distintos contra
  el tamaño del resumen, que son cosas distintas: el resumen es el patrón dominante
  **más los patrones raros**, no un top-N, así que se disparaba en una situación
  perfectamente normal. Ahora `descubrir_patrones()` expone `n_patrones_raros` y la
  guarda pregunta lo único que corresponde —si ese conjunto fue recortado por el
  tope de seis—. Sin recorte la enumeración es completa y `n_afectados` toma su
  valor real; con recorte se enumera igual y el alcance es `patrones_parciales`,
  declarado en la cobertura.
- `outliers`, `valores_no_finitos`, `ceros_no_permitidos` y
  `negativos_no_permitidos` condicionaban la enumeración a que la columna fuera
  numérica **en su tipo declarado**. Al leer un CSV como texto —el caso más común
  que hay— el perfilador infiere numérico, convierte y cuenta bien, pero la rama
  miraba un `character` y no devolvía nada: se informaban diez atípicos y no se
  nombraba ninguna fila. Ahora rastrean sobre la vista cuantitativa inferida, la
  misma que usó el detector.
- `codificacion_rota` reimplementaba la detección con una clase de caracteres más
  angosta que la del detector, de modo que un valor con el mojibake del carácter de
  reemplazo se contaba y no se nombraba —y ese vacío salía declarado con alcance
  `completo`, que es justo lo que este paquete no hace—. Ahora reutiliza la máscara
  del detector.
- `patron_raro` nombraba, además, filas que su propia evidencia acababa de
  descartar. En una secuencia entera densa, un patrón que difiere sólo por el largo
  —`9` frente a `9+`— no es un desvío: es el mismo número con menos dígitos. El
  detector lo filtraba al armar la evidencia; la rama de índices y `n_afectados`
  recorrían el resumen crudo. El conjunto filtrado se calcula ahora una sola vez y
  viaja con el resultado, de modo que no puede haber dos criterios.

**El principio que unifica los cuatro: la trazabilidad no recalcula lo que el
detector ya resolvió.** Cada vez que lo recalculaba, los dos criterios se separaban
en silencio.

**Y la prueba que faltaba.** La suite verificaba conteos, no identidades: una prueba
que comprueba `n_afectados == 10` pasa igual si el paquete nombra diez filas
equivocadas, ninguna, o seiscientas. Por eso ninguno de estos desajustes se veía con
toda la suite en verde. Ahora hay fixtures que construyen tablas con índices
corrompidos **conocidos de antemano** y verifican aciertos, falsos positivos y
pérdidas.

## El piso de asimetría del vocabulario declara lo que deja afuera

- El comparador de vocabulario abría grupos por distancia con cualquier
  desbalance de frecuencias, y así señalaba `este` frente a `oeste` —dos puntos
  cardinales— como posibles variantes de un mismo valor. Medido sobre tablas
  limpias y sobre erratas sembradas, los falsos positivos quedan entre `1,0` y
  `1,5` de asimetría y las erratas reales desde `9,0`, así que ahora se exige
  una asimetría mínima de `2`, configurable con `min_asimetria_vocabulario`.
- **El piso no se aplica a los grupos formados por normalización.**
  `Montevideo`, `MONTEVIDEO` y `Montevideo ` son tres grafías del mismo valor y
  con una aparición cada una su asimetría es `1,0`: ahí la equivalencia está
  comprobada y no es una conjetura sobre una errata.
- **Y lo que el piso deja afuera se declara.** En la banda de asimetría baja cae
  también una errata sistemática que afecte a una fracción grande de los
  registros, y por la forma es indistinguible de dos valores legítimamente
  parecidos. Elegir en silencio cuál se sacrifica sería justo lo que este
  paquete no hace: `cobertura_diagnosticos` informa cuántos grupos quedaron bajo
  el piso y cómo bajarlo.

## Colecciones: el séptimo nivel de granularidad deja de estar sólo declarado

- `coleccion()` declara qué tablas componen una base de datos, con su esquema,
  y `perfilar_coleccion()` devuelve una fila por tabla con sus agregados
  exactos, más `cobertura_coleccion` con lo que no se pudo medir. La
  granularidad `coleccion` del marco estaba declarada y no se medía: lo que
  faltaba no era código sino el objeto.
- **La frontera se declara, nunca se descubre.** Recorrer el catálogo
  convertiría un error de permisos en un resultado, y una colección real pasa de
  mil tablas repartidas en decenas de esquemas.
- **El esquema es parte de la identidad de la tabla**, así que el mismo nombre
  en dos esquemas son dos tablas y no una repetida.
- **Lo que no se pudo leer se declara y nunca queda en cero**: una tabla sin
  permiso, un objeto declarado como vista, un motor que rechaza un agregado. En
  bases institucionales los permisos parciales son el caso normal.
- **Cada tabla declara su propio muestreo**, y no se promedian alcances
  distintos como si fueran uno.
- **No hay lectura instantánea.** Perfilar una colección son muchas consultas y
  la base puede cambiar entre ellas, así que cada fila trae el `momento` en que
  se midió y `meta$snapshot` declara que no lo hubo.
- El perfil pesado de cada tabla no se retiene salvo que se pida con
  `conservar_perfiles = TRUE`: con cientos de tablas no entraría en memoria.
- `agregar()` mide ahora esta granularidad, con tres condiciones. Exige la
  **frontera declarada**, porque sin saber sobre qué tablas se agrega el número
  no significa nada. Admite **sólo `promedio_ponderado`**: sin esa restricción
  bastaba pedir `promedio` para obtener un número entre tablas de universos
  distintos sin declarar nada, que es el juicio que el paquete se niega a
  inventar. Y **la cobertura viaja pegada al número**.
- Esa última condición es la que más importa, y salió de refutar el diseño. Un
  número sobre «la colección» calculado sólo con las tablas que se pudieron
  medir **informa como medido lo que no se midió**: el peso de la tabla ausente
  desaparece en vez de manifestar la falta de cobertura. Con quince tablas
  declaradas y seis sin permiso, el número describe nueve y se presenta como si
  describiera la colección. Ahora el resultado trae `tablas_declaradas`,
  `tablas_en_el_numero`, `tablas_sin_medir` con su motivo, la `cobertura` y la
  advertencia de que leerlo sin ella sería exactamente ese error.
- `relaciones_coleccion()` busca claves foráneas candidatas entre **los pares
  que se declaren**, y `estimar_costo_coleccion()` permite ver el costo antes.
  Los pares se declaran por la misma razón que la frontera: una clave foránea es
  **dirigida**, así que mil tablas dan casi un millón de direcciones, y el costo
  real no lo da el número de tablas sino el de comparaciones entre columnas.
  Cada par se compara sobre una muestra, y el objeto declara que **una relación
  candidata sobre una muestra no es una clave foránea comprobada**: es un
  indicio que hay que confirmar contra el diccionario de datos. Un par que no se
  pudo leer se declara en `cobertura_pares` en vez de desaparecer.
- `granularidades()` declara el séptimo nivel como implementado: siete de diez.
  Los tres últimos siguen sin objeto, y no por falta de código: qué bases
  componen un conjunto y qué bases pertenecen a un organismo son decisiones de
  gobernanza que no están en ningún dato.

## Evaluar estimaciones que calculó otra herramienta

- `medicion_desde_estimaciones()` recibe estimaciones ya calculadas —por
  `survey`, por [`calidad`](https://github.com/inesscc/calidad) del INE de
  Chile, o por cualquier otra fuente— y las lleva al contrato de `medir()`, para
  poder evaluarlas contra un marco declarado. **`lupa` no estima**: eso necesita
  diseño muestral, estimación de varianza y otra disciplina; lo que sabe hacer
  es evaluar contra un marco, y eso es lo que ofrece.
- Cada estadístico se convierte en **su propia medida canónica**, con su
  métrica, su tipo, su unidad y su orientación, porque los siete tienen dominios
  distintos: un coeficiente de variación de `0,30` y un tamaño de muestra de
  `0,30` no se leen igual. `estadisticos_estimacion()` publica el catálogo.
- **La procedencia viaja en cada medida** y es obligatoria, para que nadie lea
  el resultado como si `lupa` lo hubiera calculado. Los estadísticos que la
  tabla no traiga no se rellenan con ceros: se declaran ausentes.

## Señales redundantes: la contradicción que ninguna columna muestra sola

- `senal_redundante()` declara que varias columnas de una tabla codifican el
  mismo hecho, y `detectar_discordancias()` informa las filas donde no
  concuerdan dentro de la ventana declarada. El caso típico son el año de la
  fecha, el año fiscal y el año del archivo: los tres pueden ser plausibles por
  separado y aun así contradecirse.
- **El grupo se declara, nunca se adivina.** Dos columnas de año pueden ser el
  de nacimiento y el de ingreso, y no tienen por qué coincidir; suponerlo sería
  inventar conocimiento del dominio.
- `transformacion` lleva columnas guardadas de formas distintas a una escala
  comparable —extraer el año de una fecha, por ejemplo—, y `ventana` es la
  tolerancia **en las unidades del valor comparado**, que no se adivina.
- Una fila con alguna columna ausente **no cuenta como desacuerdo**: sale del
  universo, y `n_evaluadas` lo declara. Si ninguna fila tiene todas las columnas
  presentes, `n_discordantes` queda en `NA` y la señal se declara no evaluada,
  en vez de informar cero discordancias.

## Los umbrales de una regla salen del closure y se pueden consultar

- `regla_evaluacion()` acepta `umbrales`, una lista con nombres que se le pasan
  a la condición al evaluarla. Antes el umbral quedaba encerrado en el
  *closure*: para mover un número había que escribir otra regla, y nadie podía
  consultar cuál era. Ahora la misma función evalúa distinto con dos umbrales
  —0,67 y 0,33 sobre los mismos valores— sin reconstruir la lógica.
- Una condición que no recibe un umbral declarado se rechaza enumerando los
  argumentos que sí acepta, en vez de ignorarlo en silencio. Una condición con
  `...` los recibe todos.
- **`propiedades_regla()`** muestra lo que una regla declara: métricas, nivel,
  proporción mínima, desenlace y umbrales. Es la contraparte de
  `propiedades_metrica()`, que describe métricas: un umbral pertenece a una
  regla y no cabía allí.

## Trazabilidad por clave declarada: del hallazgo que se lee al que se verifica

- `perfilar()` acepta `clave` con las columnas que identifican una fila. La
  trazabilidad de cada hallazgo trae además el valor de esas columnas para las
  filas señaladas, así que el caso se puede buscar en el sistema de origen sin
  abrir la tabla. `analizar()` lo traslada por `...` y por `argumentos_perfil`.
- La trazabilidad separa dos ejes que antes se confundían: `estado`
  —`disponible`, `truncada`, `no_disponible`, `no_aplica`— dice si se pudo
  localizar y hasta dónde, y **`localizador`** —`indice_fila`,
  `clave_declarada`, `ninguno`— dice con qué. Una trazabilidad puede ser al
  mismo tiempo por clave y truncada.
- Las claves viajan como data frame, una fila por índice mostrado y una columna
  por componente: concatenarlas perdería los tipos y haría ambigua una clave
  compuesta.
- **La clave que permite verificar es la que identifica a una persona.** Si
  alguna de sus columnas se clasifica como dato personal y la protección está
  activa, sus valores salen enmascarados igual que la evidencia, en todos los
  hallazgos —la clave viaja con la fila, no con la columna del hallazgo— y
  `claves_protegidas` declara cuáles se enmascararon.
- Una clave que nombra columnas inexistentes se rechaza enumerando las
  disponibles; una que no es única avisa y sigue, porque sirve igual para
  localizar aunque deje de ser una clave.

## La cobertura del vocabulario deja de contradecir al hallazgo

- `casi_duplicados_vocabulario` nombraba dos diagnósticos distintos: agrupar
  valores por su forma normalizada, que no depende de nada, y medir proximidad
  por distancia de edición, que necesita `stringdist`. Sin ese paquete el
  primero medía y el segundo se declaraba **bajo el mismo nombre y para la misma
  columna**, así que cruzar `cobertura_diagnosticos` con `hallazgos` por
  `(diagnostico, columna)` —el uso natural para un consumidor automático—
  devolvía una contradicción: el mismo diagnóstico declarado como no evaluado y
  reportado como medido.
- La cobertura pasa a llamarse **`proximidad_vocabulario`** en las tres razones
  que le corresponden: falta `stringdist`, el vocabulario excede el alcance de
  comparación, y el grupo candidato mayor abarca tanto que el diagnóstico no
  aplica. El hallazgo conserva su nombre. Quien filtre la cobertura por el
  nombre viejo tiene que actualizar el filtro.

## Dos cosas más que el objeto ahora declara

- `patron_raro` distingue en la evidencia las dos clases de desvío:
  `clase_desvio=largo_de_corrida` cuando el valor señalado sigue el mismo patrón
  con un número de otro largo —`persona9@` frente a `persona300@`— y
  `clase_desvio=estructural` cuando es otra forma —`SIN CODIGO` frente a
  `AB-12345`—. **La severidad no cambia**: los dos casos son indistinguibles por
  la forma y eso está medido. Lo que cambia es que quien lee el hallazgo lo
  resuelve de un vistazo en vez de comparar patrones a ojo.
- `agregar()` acepta el nombre relacional de la granularidad —`celda`,
  `columna`, `tupla`, `tabla`— igual que `metrica()`. Su propio mensaje de error
  ya los enumeraba, así que rechazarlos era una inconsistencia. El objeto sigue
  guardando el nombre canónico del marco.

## Spearman para relaciones monótonas que no son lineales

- `detectar_asociaciones()` acepta `metodo_numerico = "spearman"` y mide
  asociación monótona sobre los rangos, sin suponer linealidad. Sobre una
  relación cúbica con ruido, Pearson da 0,918 y Spearman 0,997. Pearson sigue
  siendo el valor por omisión, y el método elegido viaja en la columna `metodo`
  con su supuesto en `supuesto`, así que ninguna lectura depende de recordar
  cuál se pidió. `analizar()` lo traslada con
  `metodo_asociacion_numerica`.
- Los dos README explican ahora dónde viven la distribución de valores y las
  correlaciones —en `analizar()`, no en `perfilar()`— y por qué esa separación
  es deliberada.

## Tres afirmaciones que el paquete hacía sin fundamento suficiente

- `alta_cardinalidad` se apoyaba sólo en la tasa de valores distintos, y con
  pocas filas esa tasa está dominada por el tamaño: una columna de dos valores
  en tres filas daba 0,67 y superaba el umbral, aunque una columna de dos
  valores no puede tener cardinalidad alta. Ahora el hallazgo exige además al
  menos diez valores distintos. Las columnas con cardinalidad alta real —treinta
  valores distintos en cuarenta filas— se siguen informando igual.
- `columnas_duplicadas` afirmaba que dos columnas tienen el mismo contenido en
  tablas **sin ninguna fila**, donde dos columnas vacías coinciden sin que eso
  sea evidencia. Ese caso pasó a `cobertura_diagnosticos` con su motivo. Cuando
  sí hay filas, la evidencia declara ahora sobre cuántas se comparó.
- `relacion_aritmetica_columnas` se salteaba en silencio cuando la tabla no
  llegaba al mínimo de filas comparables. Ahora se declara en
  `cobertura_diagnosticos`, y sólo cuando había combinaciones de columnas
  numéricas que evaluar.

## El vínculo entre una acción del plan y su hallazgo ya no depende de la prosa

- `planificar_limpieza()` recuperaba el par de columnas duplicadas comparando la
  cadena de evidencia completa del hallazgo. Al enriquecerse ese texto, dos
  pares distintos colapsaban en el mismo y el plan perdía una acción. El vínculo
  se hace ahora contra el primer tramo de la evidencia, que es el que identifica
  el par.

## El perfilado no toca los datos, y ahora está probado

- Ninguna función de análisis altera la tabla que recibe: ni sus valores, ni sus
  tipos, ni sus nombres, ni sus atributos. Una prueba de regresión lo verifica en
  `perfilar()`, `analizar()`, `detectar_claves()`, `detectar_dependencias()`,
  `distribucion_valores()`, `detectar_asociaciones()`,
  `detectar_duplicados_aproximados()`, `planificar_limpieza()` y
  `guiar_limpieza()`. El caso que importa es `data.table`, que R permite
  modificar por referencia: la prueba compara además la dirección de memoria del
  objeto. `aplicar()` devuelve una copia y deja intacta la original.

## Duplicados: el hallazgo no afirma una igualdad que produjo la normalización

- La normalización por omisión iguala mayúsculas, espacios, acentos y comillas,
  así que un par puede coincidir después de normalizar sin que los valores
  guardados sean iguales. `tipo_par` distingue ahora `exacto`,
  `exacto_normalizado` y `aproximado`; `igualo_normalizar` deja esa causa
  visible en cada fila. El hallazgo `duplicados_exactos_normalizados` evita
  afirmar que dos filas tienen los mismos valores y la trazabilidad lo busca
  entre los pares de ese tipo. `n_pares_exactos` cuenta sólo texto guardado
  igual y `n_pares_exactos_normalizados` completa la explicación junto con
  `n_pares_aproximados`.

## Casi-claves y precedencia de ausencias

- `perfilar()` informa una `casi_clave` cuando una columna tiene al menos 100
  filas, supera 90 % de valores distintos y al menos la mitad de sus duplicados
  excedentes se concentra en un valor. Las fechas y fecha-hora se excluyen por
  su rol propuesto. La evidencia enumera las colisiones, sus frecuencias y los
  criterios aplicados. Los vectores `double` con algún valor finito
  fraccionario se excluyen, mientras que los formados por valores enteros se
  conservan para admitir identificadores importados desde archivos de texto.
  Los vectores `integer64` cuentan como enteros semánticos. `detectar_claves()`
  las expone sin confundirlas con claves exactas, y `analizar()` las reitera en
  sus advertencias.
- `casi_duplicados_vocabulario` retira primero los valores ya detectados como
  `faltantes_disfrazados`. Un centinela de ausencia deja de presentarse como
  posible errata de otro valor; las variantes que no son centinelas conservan
  el diagnóstico.

## Tablero, indice declarado y medicion agregada

- `tablero_calidad()` resume una corrida por metrica y objeto, declara la
  agregacion aplicada en cada fila y conserva el alcance completo del marco.
- `indice_calidad()` no calcula nada sin pesos del usuario. Con una declaracion
  completa conserva cobertura, pesos por dimension, combinaciones internas,
  inversiones de defectos, exclusiones `no_aplica` y la advertencia de que los
  componentes provienen de universos distintos.
- `analizar()` mide por omision la propuesta en estado `lista`, declara que no
  fue confirmada, agrega las medidas y conserva el tablero. El detalle fila a
  fila solo se retiene con `conservar_detalle_medicion = TRUE`; la medicion
  automatica se desactiva con `medir_propuesta = FALSE`.

## Secuencias enteras densas y vocabularios breves

- El perfil de columna publica si los enteros observados cubren densamente su
  rango, junto con densidad, posiciones y huecos. En esa condicion los
  centinelas numericos y los desvios que solo expresan el largo de una corrida
  de digitos no interpretan el contenido del identificador; los ausentes,
  duplicados y restantes diagnosticos siguen activos. Una secuencia densa y
  unica se presenta como `posible_identificador` y no recomienda convertir el
  texto numerico a una medida cuantitativa.
- `casi_duplicados_vocabulario` cubre una sustitucion en valores de hasta seis
  caracteres cuando la variante ocupa como maximo `0.05` de la columna y la
  forma dominante es al menos `10` veces mas frecuente y ocupa al menos `0.5`
  de la columna. El limite y los tres umbrales quedan en la evidencia y se
  pueden ajustar en `perfilar()`.

## Orientacion explicita de las metricas

- `metrica()` declara si un resultado expresa `"conformidad"`, `"defecto"` o
  `"no_aplica"`. La orientacion viaja por `medir()`, `agregar()`, `reportar()` y
  `evaluar()` sin invertir los valores; una regla puede recibirla como segundo
  argumento. El historico conserva el esquema 1 y sigue leyendo archivos
  anteriores.
- `Formato` queda alineada con el factor `Correctitud sintactica` de
  `marco_agesic()`, y una prueba contrasta todos los pares dimension-factor del
  nucleo contra el marco.

## Marco CEA/CEPAL de aseguramiento de la calidad

- `marco_cepal()` incorpora los cuatro niveles y diecinueve principios del
  marco nacional de aseguramiento de la calidad de las Naciones Unidas,
  adoptado y adaptado para América Latina y el Caribe por la CEA/CEPAL. Los
  principios 1 a 13 quedan declarados fuera del alcance de una tabla; los
  principios 14 a 19 quedan disponibles para documentar productos estadísticos,
  sin afirmar que el profiling genérico los mida.

## Severidad del vocabulario y escala de las relaciones

- `casi_duplicados_vocabulario` queda como señal `sospechoso` sólo cuando
  encuentra grupos; un resultado negativo queda como `ok` con cero afectados,
  y un diagnóstico que no aplica se registra en `cobertura_diagnosticos`.
- `relacion_orden_columnas` separa la escala de la relación fila a fila con un
  solapamiento intercuartil mínimo de `0.1`. Una brecha con IQR cero conserva
  una relación estable aunque los rangos no se solapen; ambos criterios y los
  pares descartados o recuperados quedan en el alcance.

## Perfil de una muestra DBI con universo explícito

- Se agrega `perfilar_dbi()` para separar los agregados SQL exactos sobre una
  tabla completa del perfil de 99 campos calculado sobre una muestra declarada.
  La salida registra el motor informado por DBI, cada consulta, los agregados no
  disponibles y la reproducibilidad efectiva del orden, sin escribir en la base.

## Desenlaces declarados por reglas

- `regla_evaluacion()` acepta `desenlace = "suprimir"` para que una regla
  declarada por el usuario produzca un plan sobre las medidas que no cumplen su
  condición. La evaluación conserva objeto, valor medido, motivo y regla sin
  modificar la medición ni los datos de origen. Sin esa declaración no crea
  desenlaces ni aplica umbrales de publicación.
- `reportar()` enmascara los valores alcanzados por ese plan tanto en la
  evaluación como en las mediciones incluidas en el mismo reporte. El
  enmascarado se hace sobre copias usadas para renderizar.

## Ley de Benford con aplicabilidad explícita

- `perfilar()` evalúa la ley de Benford solamente en columnas numéricas con
  suficiente evidencia inicial. Antes de comparar exige variación, ausencia de
  apariencia de identificador (incluidas secuencias correlativas), al menos
  100 valores positivos, todos los valores finitos positivos y tres órdenes de
  magnitud. Las precondiciones y
  sus umbrales quedan en `meta$benford`; las que fallan se declaran en
  `cobertura_diagnosticos` y no producen hallazgos.
- Cuando aplica, el perfil conserva la distribución observada y esperada por
  primer dígito, el chi-cuadrado de Pearson y su valor p. Una desviación se
  presenta como señal descriptiva para revisar, nunca como acusación de fraude
  o manipulación.

## URLs, unidades y celdas multivaluadas

- `validar_url()` valida de forma vectorizada URLs `http` y `https`, con
  esquema obligatorio por omisión, soporte para IDN y puertos, y rechazo
  deliberado de `javascript:`, `data:`, espacios y controles literales.
- `perfilar()` informa `unidades_mixtas` cuando una columna numérica escrita
  como texto combina sufijos de unidad, conservando sus frecuencias y sin
  convertir datos. Reconoce además monedas como prefijos o sufijos y emite
  `monedas_mixtas` con sus frecuencias, sin convertir ni suponer tasas de
  cambio. También informa `celdas_multivaluadas` sólo cuando las partes
  homogéneas pasan el control de patrones y tipo, incluidos identificadores
  numéricos con puntuación interna; nombres y direcciones con comas no se
  presentan como listas.

## Relaciones aritméticas entre columnas

- Reconoce una regularidad mediante un único soporte declarado
  (`umbral_aritmetica = 0.9`) dentro de la tolerancia y, una vez reconocida,
  informa todas sus discrepancias sin aplicar un segundo filtro por su cantidad
  absoluta: `max_violaciones_aritmetica` se elimina. El soporte, el universo
  mínimo y la tolerancia quedan en la evidencia y el alcance.
- `perfilar()` descubre identidades aditivas y proporcionalidades estables
  entre columnas numéricas y las presenta como evidencia observada, no como
  reglas del dominio. Cada hallazgo declara proporción de cumplimiento,
  universo de filas finitas, tolerancia numérica, constante proporcional y
  filas discrepantes.
- `umbral_aritmetica`, `min_filas_aritmetica`, `tolerancia_aritmetica` y
  `max_columnas_aritmetica` hacen visibles los supuestos y el costo del
  diagnóstico. Si el límite de columnas recorta combinaciones,
  `cobertura_diagnosticos` lo declara explícitamente.

## Capa de marcos

- `regla_evaluacion()` acepta `proporcion_minima` para declarar un veredicto
  sobre la proporción de medidas que cumple la condición. El objeto conserva el
  umbral; la evaluación muestra proporción, veredicto, componentes y universo,
  sin ponderar medidas ni crear un puntaje global. Las reglas por medida
  conservan su contrato y su estructura de salida.
- `metrica()` acepta las etiquetas relacionales de `granularidades()` —por
  ejemplo, `"columna"`— y guarda siempre su equivalente canónico de la
  ontología (`"atributo"`). Un valor inválido muestra ambos vocabularios.
- El error de una regla que no engancha ninguna medida enumera lo solicitado y
  las métricas instanciadas disponibles, incluidos sus nombres calificados.

## Perfilado de geometrías

- Las columnas `sfc` informan CRS, tipo de geometría, geometrías vacías e
  inválidas, coordenadas fuera del dominio declarado y caja envolvente. Una
  geometría sin CRS deja el conteo de dominio en `NA`: no se supone EPSG:4326.
  Las geometrías vacías se cuentan aparte y no integran el universo del chequeo
  de dominio.
- Los tipos mixtos se comparan por familia: las variantes simples y `MULTI`
  compatibles conviven sin hallazgo, mientras que familias distintas y
  `GEOMETRYCOLLECTION` se señalan. La validez declara
  `validez_criterio = "planar"`; sobre CRS geográficos un fallo planar es
  sospechoso y no afirma invalidez esférica.
- `n_dominio_evaluados` y `n_bbox_evaluados` hacen públicos los universos no
  vacíos de sus métricas; `bbox_alcance` declara que la caja usa las coordenadas
  crudas, incluidas las que estén fuera de dominio. `n_validez_evaluados`
  publica por separado el universo de GEOS, incluidas las geometrías vacías.
  `dimension_geometria` declara `XY`, `XYZ`, `XYM` o `XYZM`; Z y M quedan
  enumeradas en `dimensiones_no_evaluadas` y generan una fila de cobertura.
  Para `XYM` y `XYZM`, la validez topológica se calcula en XY después de
  `st_zm()` y `validez_preprocesamiento` declara ese paso.
- El control de dominio compara también las coordenadas transformadas con la
  `BBOX` del área de uso del WKT. Detecta, entre otros casos, grados donde el
  CRS espera metros; no detecta una zona UTM equivocada cuando las coordenadas
  interpretadas caen dentro del área de esa zona. Una caja mundial es un no-op
  evaluado y un WKT sin `BBOX` produce una fila de cobertura, sin asumir alcance
  global.
- Los nuevos hallazgos distinguen CRS ausente, geometrías inválidas o vacías,
  coordenadas imposibles y tipos geométricos mixtos. Si falta el paquete
  opcional `sf`, el perfil no inventa ceros ni hallazgos: registra una fila con
  `dependencia = "sf"` en `cobertura_diagnosticos`.

## Fechas con meses escritos

- `detectar_formatos_fecha()` reconoce fechas con meses escritos en español
  (incluye `setiembre` y `set`) y en inglés, además de los formatos numéricos
  existentes. La tabla de nombres es propia y no depende de `LC_TIME`, y sólo
  acepta la estructura completa de una fecha o de un mes con año: encontrar
  `marzo` dentro de una oración no convierte el texto en fecha. Los meses
  escritos desambiguan el día y el mes; los años de dos dígitos siguen siendo
  candidatos y no se les asigna un siglo en silencio.
- Los períodos expresados sólo como mes y año declaran `granularidad = "mes"`
  y no inventan el día 1 para calcular mínimos, medias o conversiones; esos
  resúmenes quedan en `NA` con estado `granularidad_incompleta`. Los años
  escritos en meses también se limitan al rango 1800--2100, como las fechas
  compactas.
- La detección de meses sólo ejecuta sus expresiones regulares sobre los
  valores candidatos y reutiliza ese resultado al calcular el resumen de la
  columna. Así el texto libre que menciona meses no paga el costo completo ni
  se vuelve a analizar.
- Ese resultado intermedio se mantiene sólo durante el perfilado y no queda
  adjunto al objeto público `formatos_fecha`. En columnas mixtas, los resúmenes
  de fecha se calculan sobre las fechas completas y declaran cuántas fechas de
  mes-año quedaron fuera; una columna compuesta sólo por períodos conserva el
  estado `granularidad_incompleta`.
- `inferir_tipo()` tampoco conserva el caché interno de detección de meses.
  El diagnóstico de variantes del vocabulario sigue siendo una señal
  heurística: Jaro--Winkler puede acercar nombres de calles o códigos con
  prefijos compartidos y sus grupos deben revisarse como sospechosos, no como
  identidades.
- El hallazgo de variantes del vocabulario sólo atribuye el límite de proporción
  cuando existe un grupo compatible que retener; si todas las cercanías fueron
  descartadas por secuencias numéricas incompatibles, lo informa con ese
  motivo.

## Variantes del vocabulario

- `perfilar()` agrega el hallazgo `casi_duplicados_vocabulario`: agrupa, por
  columna, variantes que la normalización funde o que quedan bajo el umbral de
  Jaro--Winkler y conserva la frecuencia de cada forma. La unidad es el valor
  distinto, no la fila; no se elige una forma canónica ni se modifica el dato.
  Las aristas de distancia forman estrellas alrededor de un valor de frecuencia
  estrictamente mayor y único; los empates no se fuerzan y no se cierra
  transitivamente una cadena de vecinos. Cada grupo declara su distancia mínima
  y máxima, y `max_proporcion_grupo_vocabulario`
  permite declarar que el diagnóstico no aplica cuando un componente abarca
  demasiado vocabulario; el filtro se activa desde 20 valores distintos o
  cuando el grupo mayor tiene al menos 10 variantes, y sólo suprime si la
  proporción también supera el umbral. Así no oculta grupos pequeños, pero
  tampoco entrega una columna entera como una sola familia. Cuando hay pares
  cercanos pero no una frecuencia central única, el alcance declara la falta de
  asimetría y apunta a `detectar_duplicados_aproximados()`. Las aristas de
  distancia con secuencias numéricas distintas se descartan; los ceros de
  relleno y separadores de miles se consideran equivalentes, pero una errata
  dentro de un número puede quedar sin agrupar deliberadamente. El alcance
  informa los pares descartados por números y separa el tamaño potencial del
  componente del tamaño que queda compatible con esa regla.
  `casi_duplicados_vocabulario = FALSE` lo desactiva.
  El alcance declara los valores y pares comparados, los recortes y la ausencia
  de [`stringdist`](https://cran.r-project.org/package=stringdist); las
  fusiones exactas se siguen informando sin ese paquete.
  Los resultados del perfil pueden cambiar porque ahora se señalan estas
  variantes como evidencia para una revisión de vocabulario.

## Referenciales

- Las métricas de referenciales heredan el perfil de `normalizar` declarado en
  `referencial()` (o aceptan uno explícito), por lo que variantes de caja,
  acentos y espacios pueden pasar a reconocerse como presentes. Esto cambia
  los resultados de correctitud y cobertura de forma deliberada; las claves
  siguen evaluándose por identidad exacta.
- `CorrectitudSemFuerte` y `CorrectitudSemDebil` pueden agregar, sin cambiar el
  veredicto, el candidato más cercano y su distancia como evidencia. La
  proximidad usa Jaro--Winkler por omisión (`p = 0.1`, umbral `0.10`), sólo se
  calcula para fallos y declara sus límites o la ausencia de
  [`stringdist`](https://cran.r-project.org/package=stringdist). Se calcula
  sobre los valores fallidos distintos y se reparte a las filas repetidas; el
  alcance distingue filas fallidas, valores distintos y valores comparados.

## Perfil de normalización para comparar

- `normalizar` deja de ser sólo un interruptor lógico: `TRUE` conserva el caso
  común con minúsculas, espacios, acentos protegidos y comillas; `FALSE`
  desactiva esos pasos configurables; `"amplio"`, [normalizacion()] y una lista
  nombrada permiten elegirlos por columna. La representación normalizada sólo
  decide qué valores se comparan: nunca modifica los datos guardados.
- `perfilar()` conserva el perfil resuelto y los análisis de duplicados y claves
  lo heredan cuando reciben `normalizar = NULL`. Los resultados pueden cambiar
  porque el umbral se aplica sobre la cadena normalizada; el perfil informa,
  por vocabulario, cuántos valores fundió cada paso.
- La comparación aplica siempre descomposición y orden canónicos en el
  subconjunto latino cubierto; no reordena palabras ni aplica abreviaturas de
  vías. Las claves siguen descubriéndose por identidad exacta y agregan la
  unicidad normalizada como métrica informativa.
- El informe de fusiones compara el perfil completo con una versión que apaga
  cada paso por separado: sus cifras no son aditivas y el total normalizado se
  informa aparte. Ahora usa el vocabulario completo (las fusiones son una
  propiedad de pares que una muestra de valores puede ocultar) y la
  normalización se aplica de forma vectorizada; `n_usados` y el estado `exacto`
  dejan explícito el alcance real.
- El informe de fusiones no se calcula cuando `normalizar = FALSE`, porque no
  hay pasos configurables que evaluar. Cuando `perfilar()` ya lo calculó,
  `detectar_duplicados_aproximados(perfil = ...)` lo reutiliza en lugar de
  recorrer de nuevo el vocabulario.
- `proteger` acepta grafemas compuestos y el valor predeterminado conserva
  `g̃` además de `ñ` y `ü`, para no borrar letras guaraníes al comparar.

## Diagnósticos de texto invisible

- Amplía la detección a los espacios Unicode, marcas direccionales, BOM y
  otros invisibles de transporte. Los espacios Unicode se pueden colapsar a
  espacio ASCII sólo mediante una acción explícita y destructiva; ZWJ/ZWNJ se
  informan pero se conservan. La comparación normalizada usa estas mismas
  clases sin borrar caracteres semánticamente significativos.
- El hallazgo de separadores en campo, su acción y su conteo usan nombres
  específicos para cubrir tabulaciones, saltos, avances de página y tabulaciones
  verticales.
- `perfilar()` identifica controles C0/C1 e invisibles Unicode, entidades HTML
  reconocibles y separadores dentro de campos. La evidencia escapa esos
  caracteres (`<U+200B>`, `\\t`, `\\n`, `\\r`, `\\f`, `\\v`) y conserva los
  conteos por fila.
- Los controles invisibles que no son separadores se pueden eliminar y se
  recomiendan por defecto; decodificar entidades HTML y reemplazar separadores
  de línea quedan como acciones explícitas porque pueden cambiar contenido
  legítimo. Las tres dejan el número de valores cambiados en el registro.

## Reparación de texto y licencia

- La medida predeterminada de duplicados ahora aplica Jaro--Winkler con
  `p = 0.1` (el valor anterior era Jaro puro por `p = 0`) y el umbral pasa de
  `0.12` a `0.10`. Las dos decisiones pueden cambiar los pares informados al
  actualizar; el cambio es deliberado y queda declarado en la ayuda.

- Declara `cli (>= 3.0.0)`. El motor usa la interfaz de barras de progreso
  (`cli_progress_bar()` y sus compañeras), que existe recién desde esa
  versión; antes el requisito estaba supuesto y no escrito.

- Clasifica los duplicados exactos comparando los textos que realmente entran a
  la medida, después de normalizarlos, y no mediante igualdad exacta de un
  flotante. Esto hace el resultado independiente de la arquitectura y mantiene
  como `aproximado` un par de textos distintos aunque `soundex` devuelva
  distancia cero.

- Cierra el motor de reparación de texto: `decode_inconsistent_utf8` trabaja
  por subcadenas con el detector de [ftfy 6.3.1](https://github.com/rspeer/python-ftfy),
  conserva los estados parciales con U+FFFD y agrega tres extensiones
  deliberadas de badness sobre ftfy 6.3.1: la regla de inicio del issue
  [#222](https://github.com/rspeer/python-ftfy/issues/222), también discutida en
  el [PR #232](https://github.com/rspeer/python-ftfy/pull/232); la regla de caja
  que detecta mojibake de KOI8-R del issue
  [#231](https://github.com/rspeer/python-ftfy/issues/231); y la regla específica
  para `â` del issue [#233](https://github.com/rspeer/python-ftfy/issues/233).
  La tabla de bytes KOI8-R es la cuarta extensión y la puerta literal `Ã ` para
  formas portuguesas y francesas es la quinta.
- Incorpora un motor R puro para detectar y reparar mojibake en varias
  codificaciones, inspirado en el diseño y las tablas de [ftfy
  6.3.1](https://github.com/rspeer/python-ftfy) de [Robyn
  Speer](https://github.com/rspeer). Los resultados distinguen reparaciones completas, parciales y casos
  irrecuperables; los estados llegan al hallazgo, al plan y al registro.
- Completa el port de las reglas de detección y de los transcodificadores de
  [ftfy](https://github.com/rspeer/python-ftfy): las transformaciones de bytes
  se encadenan antes de decodificar, las pérdidas quedan como U+FFFD y estado
  `reparado_parcialmente`, y nunca se introduce un control invisible nuevo.
- Completa `restore_byte_a0` de [ftfy 6.3.1](https://github.com/rspeer/python-ftfy):
  conserva la frontera de la palabra `à`, respeta las excepciones portuguesas
  y cubre las seis formas de bytes alterados, sin partir ni pegar palabras.
- Conserva los espacios no separables y agrega el decodificador R puro de
  variantes UTF-8 de [ftfy](https://github.com/rspeer/python-ftfy): combina
  pares CESU-8 y reconoce `C0 80`, e incorpora la tabla de bytes KOI8-R
  adicional, con los estados y pérdidas ya declarados.
- Declara como quinta extensión deliberada la puerta adicional para la secuencia
  literal `Ã `, que conserva las formas portuguesas y francesas observadas en
  padrones; el decodificador de variantes rechaza secuencias que producirían un
  NUL, en vez de omitir un carácter al materializar el texto.
- La licencia del paquete pasa de `GPL-2 | GPL-3` a `GPL-3`; las partes
  derivadas del diseño de [ftfy](https://github.com/rspeer/python-ftfy) se
  atribuyen en `LICENSE.note` bajo Apache-2.0.
- La estrategia de reparación de texto se registra como `reparar_codificacion`.

## Recursos de comparación

- Fija por omisión en dos los hilos que [`stringdist`](https://cran.r-project.org/package=stringdist) puede usar en las
  comparaciones aproximadas y declara el valor efectivo en `alcance`.
- El aviso interactivo del camino LSH identifica `nucleos` como la perilla que
  puede acortar la etapa de comparación, sin prometer una ganancia fija.
- La viñeta de escala documenta el rendimiento observado entre dos y treinta y
  un hilos y deja explícito que después de dieciséis no hubo una mejora medida.
- Documenta que el piso de tiempo de LSH cubre sólo la comparación de cadenas,
  no la firma, las cubetas ni el troceo; los resultados no dependen de la
  cantidad de hilos.
- Actualiza las mediciones de escala para anotar la configuración de hilos y
  evita presentar tiempos dependientes de la máquina como cifras exactas.

## Marcos declarables y alcance internacional

- Incorpora validadores vectorizados de ISO 3166, ISO 4217, correo, Luhn y
  módulo 97, junto con un pack uruguayo de cédula y RUT. Los packs territoriales
  se pueden extender sin registrar estado global ni modificar el núcleo.
- Separa clasificar de proteger datos personales: las formas numéricas poco
  discriminantes se informan sin suprimir estadísticos, mientras nombres
  semánticos, correos y documentos verificados conservan la protección.
- Documenta los contratos de todos los puntos de extensión y añade
  `propiedades_metrica()` para consultar la configuración admitida sin
  inspeccionar closures.
- Incorpora `marco_iso25012()` como adaptación opcional y explícita de las
  quince características de ISO/IEC 25012:2008.
- Identifica el marco activo en cada fila de `cobertura_analisis()`.
- Permite declarar taxonomías dimensión-factor con `marco_calidad()`, validar
  modelos contra ellas y calcular cobertura con AGESIC sólo como valor de
  fábrica mediante `marco_agesic()`.
- Permite construir familias de madurez con nombres y umbrales propios sin
  cambiar los tres perfiles incluidos.
- Hace que el vector de sentinelas numéricos sea una política completa:
  `numeric()` los desactiva explícitamente.
- Reconoce coma y punto decimal, separadores de miles simétricos, símbolos
  monetarios y prefijos con forma de código ISO 4217.
- Clasifica RUT, DNI y otros documentos con la etiqueta neutral
  `documento_identidad`.
- Permite conectar packs personales territoriales al mismo clasificador de
  `perfilar()`, con tolerancia explícita de errores de digitación; los nombres
  semánticos (`telefono`, `fecha_nacimiento`, entre otros) conservan prioridad
  sobre formas numéricas genéricas.

## Examinar datos

- Detecta relaciones de orden sospechosas entre columnas numéricas o temporales
  comparables (por ejemplo, `inicio <= fin` y `monto_bruto <= monto_neto`).
  El hallazgo conserva los conteos y las filas fuera de orden, sugiere
  formalizar la regla con `ReglaIntegridadIntraEntidad` y declara en
  `meta$orden_columnas` las columnas y pares efectivamente comparados. Expone
  un filtro opcional de solapamiento intercuartil para tablas anchas;
  está apagado por omisión (umbral `0`) porque activarlo puede ocultar
  relaciones reales entre magnitudes de rangos distintos. Los pares
  descartados quedan contados en el alcance.

- Protege los estadísticos de orden y cuantiles de columnas personales, marca
  cada supresión en el objeto y conserva alertas de plausibilidad para fechas de
  nacimiento sin publicar sus extremos.
- Añade `datos_operativos`, un segundo conjunto sintético y neutral, reproducible
  desde `data-raw/`, con problemas de calidad sembrados.
- Añade `analizar()` como puerta de entrada al recorrido descriptivo, con
  cobertura conceptual y advertencias de alcance en el propio objeto.
- Incorpora distribuciones de valores acotadas, cuantiles, asociaciones de
  Pearson, V de Cramér y eta cuadrado, además de regularidad, duplicación,
  monotonicidad, cobertura, días de semana y huecos temporales.
- Propone escalas de medición y roles sin confirmar lo que sólo se infiere de
  los valores; conserva niveles declarados, observados y ausentes.
- Perfila tablas administrativas con métricas generales y por columna,
  proporciones en `[0, 1]` y hallazgos filtrables.
- Descubre patrones de formato, tipos implícitos, formatos de fecha mixtos y
  ambiguos, años de dos dígitos, números regionales y problemas de codificación.
- Detecta claves candidatas, relaciones, cobertura referencial, columnas y
  filas duplicadas, y dependencias funcionales exactas o aproximadas.
- Conserva la ambigüedad día/mes con barra, guion y punto; reconoce fracciones
  de segundo y offsets ISO 8601.
- Distingue NaN e infinitos, evita aproximar `integer64` fuera del rango exacto
  de `double` y cuenta valores distintos en columnas de listas y geometrías.
- Clasifica posibles datos personales sin juzgar su presencia y protege por
  defecto los valores concretos cuando la evidencia es discriminante.
- Normaliza factores a texto sólo en la operación, conserva `factor` en el
  perfil y devuelve texto al transformar columnas factor con `aplicar()`.
- Mantiene claves históricas estables en R 3.6 y fija explícitamente en UTC las
  fechas convertidas desde `Date`.
- Las claves históricas tratan el texto ilegible (UTF-8 inválido) como ausente:
  comparte con `NA` la marca `~`, en vez de intentar codificarlo como texto
  literal.
- Añade conteos explícitos de evaluados y afectados, con la unidad de conteo,
  a cada hallazgo; conserva NA cuando el alcance no permite conocerlos.
- Añade trazabilidad acotada por hallazgo mediante índices de fila, con estados
  explícitos para lo disponible, truncado, no aplicable y no disponible; el
  reporte resume el estado sin imprimir los índices.

## Medir y evaluar calidad

- Declara métricas genéricas, específicas e instanciadas con tipo de resultado
  y granularidad explícitos.
- Incluye veintiuna métricas automatizables, tres métricas tabulares basadas en
  referenciales y una correspondencia verificable con las 49 entradas del
  catálogo de AGESIC.
- Separa en el catálogo la disponibilidad de cada métrica de la causa o el
  matiz de esa disponibilidad, y documenta las 49 correspondencias sin vacíos.
- Ajusta las métricas oficiales de oportunidad al resultado booleano del marco
  y conserva la fórmula continua del curso CPAP bajo nombres `GradoOportunidad*`.
- Incorpora contratos explícitos `vigencia()` y `escala()`, y una tabla de
  cobertura que distingue lo medido, no declarado, no aplicable y fuera de
  alcance.
- Implementa las cuatro agregaciones del marco y la cadena de evaluación de
  medidas, reglas y perfiles de madurez. No calcula un índice global.
- Propone modelos editables a partir del perfil sin convertir observaciones de
  una sola entrega en requisitos silenciosos.
- Estima el costo antes de comparar, aplica un presupuesto de pares en los
  caminos exhaustivo y LSH, y publica el alcance de la estimación.
- Incorpora MinHash y LSH deterministas para generar candidatos a escala,
  con deduplicación por banda, garantía declarada y degradación explícita.
- Permite bloquear por una columna elegida por el usuario y estima los pares
  que el bloqueo puede dejar fuera, incluidos los ausentes como bloque propio.

## Mejorar y monitorear

- Construye planes de limpieza editables con alternativas mutuamente
  excluyentes, justificación, modo guiado opcional y consentimiento adicional
  para eliminaciones.
- Aplica sólo acciones activas sobre una copia, conserva un registro y permite
  imputaciones confirmadas mediante dependencias funcionales exactas.
- Acumula evaluaciones en un histórico plano y versionado; detecta deriva del
  modelo y cambios estructurales entre perfiles.
- Procesa comparaciones exhaustivas por lotes con parciales en un directorio
  declarado, cruza los lotes sin pérdida de pares y deja constancia de que no
  son reanudables.

## Informar

- Guarda y recupera análisis versionados sin datos de entrada por omisión y sin
  serializar entornos completos de reglas funcionales.
- Genera un único HTML autocontenido, en español, sin navegador, LaTeX ni
  recursos externos; los valores se escapan y la evidencia personal se
  enmascara por defecto.
