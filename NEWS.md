# lupa 0.1.1

## Perfilado

- `columnas_duplicadas` publica un hallazgo por **grupo** de columnas con el
  mismo contenido, no uno por par: `columna` es la primera del grupo en el orden
  de los datos, la evidencia las nombra a todas (`A = B = C; comparadas sobre 5
  filas`) y `n_afectados` las cuenta a todas, como `filas_duplicadas` con sus
  filas. Con dos columnas el hallazgo es el de siempre. Si un nombre no se puede
  leer suelto —vacío, repetido, con espacios al borde o con `=`, `;`, `,` o
  comillas—, la evidencia cita todos los del grupo y el repetido lleva su
  posición (`"X" (columna 1) = "X" (columna 2)`). Dos columnas son iguales en lo
  que el perfil lee como contenido: dos factores sin orden con los mismos
  niveles en otro orden son la misma columna, y un entero y un doble no. Sin
  filas no se compara: no hay hallazgo, y `general$columnas_duplicadas`, que
  tiene una fila por columna repetida junto a la primera de su grupo, queda
  vacía. `planificar_limpieza()` propone una decisión por grupo —marcarlo, o
  eliminar las repetidas y conservar la primera— y `comparar_perfiles()` compara
  los grupos por las igualdades que afirman: si C deja de ser igual a `A = B`,
  publica un resuelto y no dos; si un grupo se separa sólo porque una columna
  cambió de tipo declarado, publica `no_evaluado`, porque el diagnóstico no
  volvió a compararlas; y un perfil guardado con la forma por pares se compara
  sin deriva falsa. Con 1.000 columnas iguales eran 499.500 hallazgos:
  `planificar_limpieza()`, `comparar_perfiles()` y `analizar()` no terminaban en
  5 minutos y ahora tardan 2, 3 y 22 s, y `print()` del perfil pasa de 37 s a
  0,6 s.

## Documentación

- Los guiones de `benchmark/` que publican cifras dejan su CSV en
  `benchmark/datos/` con fecha, entorno y huella del código; el de Raha agrega el
  SHA-256 de cada insumo, y el del tope de largo mide Jaro-Winkler con el mismo
  `p` que el paquete (el margen publicado para `max_largo_valor = 10000` era
  mayor que el real).
- El README muestra una captura del informe HTML de `reportar()`, rehecha con
  `scripts/capturar-informe-html.R`. «El vacío por diseño» y «Marcos y modelos
  de calidad» empiezan por el ejemplo y dejan la explicación para después.
- La guía «Escala y duplicados aproximados» termina en los lotes sin pérdida y
  resume cada tope en una línea. El detalle de los topes y del aviso de tabla
  ancha pasa a «Motores y escala», y el de los hilos a «Evidencia y rendimiento».
- Las 6 guías que trae el paquete y los 10 artículos del sitio tienen frases más
  cortas, cifras en dígitos y listas donde había enumeraciones, con la misma
  información y el mismo código. Los artículos van en impersonal.
  `CONTRIBUTING.md` reúne las reglas de redacción de la documentación.
- Las cifras de los artículos que el paquete define (umbrales, motores de la
  matriz, dependencias, métricas, topes) se calculan al construir el sitio, no se
  escriben a mano. Al atarlas salieron 2 frases falsas en «Alcance y cobertura»:
  sin `bit64` el perfil de una columna `integer64` se detiene con un error, no
  deja 5 medidas en `NA`; y un documento sin su dígito verificador necesita 7
  cifras, no 6. La tabla de motores del README nombra también PostgreSQL 9.3.
- El sitio tiene una versión en inglés de 4 guías («Guides in English»):
  *Getting started*, *Profiling a database*, *Engines and scale* y *From
  findings to metrics*. Viven sólo en el sitio, y `data-raw/comparar-guias.R`
  comprueba antes de construirlo que cada una tenga los mismos trozos, el mismo
  código y las mismas cifras en línea que su pareja en castellano.
- El logo y los íconos del sitio usan el hexágono regular: lados iguales y
  bordes a 30°. El anterior era más alto que ancho de la cuenta.

## Rendimiento

- Las tablas chicas se arman sin el costo por columna de `data.frame()`, la
  detección de fechas no prueba los formatos que no pueden casar con ningún
  valor, y `analizar()` clasifica una sola vez los datos personales de la tabla
  que mide. Con la protección pedida, el barrido que decide qué campos guardan
  nombres de columna no intenta partir un valor sin coma, y la fila de una
  columna del perfil se arma sin pasar por `[.data.frame`. El pliegue con que
  la protección compara los valores protegidos con el texto publicado ya no
  arma su tabla de tres mil caracteres en cada llamada: un texto ASCII sólo
  baja la caja, y uno corto usa la parte del mapa que trae. Sobre la tabla de
  ejemplo de 13 filas, `perfilar()` tarda 0,11 s y
  `analizar()` 0,21 s en una sesión ya iniciada, y 0,19 s y 0,34 s en la primera
  llamada de una sesión nueva. Con la protección pedida, `perfilar()` tarda
  0,23 s y 0,37 s, y `analizar()` 0,60 s y 0,78 s. Las cifras son de
  `benchmark/medir_costo_fijo.R` y quedan en `benchmark/datos/medir-costo-fijo.csv`.
- `perfilar()` sobre tablas anchas con muchas columnas iguales o de códigos de
  texto es varias veces más rápido. La búsqueda de casi duplicados del
  vocabulario no calcula la mitad de la matriz de distancias que no lee, y
  `columnas_duplicadas` publica un hallazgo por grupo (ver «Perfilado»). Las columnas iguales se buscan agrupándolas
  por una huella y comparando cada una con un representante, no par por par.
  Con 500 filas y 1.000 columnas iguales, de 379 a 387 s pasa a 28 a 30 s (los
  2 CSV fechados de `benchmark/medir_topes_valores_ancho.R` en
  `benchmark/datos/`).
- `perfilar()` repara una sola vez cada valor mal codificado: lo reparaban, por
  separado, el diagnóstico de la codificación y el armado de los hallazgos. En
  una columna de 100.000 valores distintos con mojibake tarda una cuarta parte menos.

## Requisitos

- R 4.2.0 o posterior. Las columnas `integer64` necesitan además bit64, desde
  la 4.0.5. Con bit64 anterior a 4.6.0, `match()` de R iguala entre sí todos los
  valores negativos: `lupa` empareja esos valores por su texto, con su propio
  `match()` y su `%in%`, y da el mismo perfil que con bit64 nuevo. Sin bit64 se
  niega con la condición `lupa_error_integer64` en lugar de publicar distintos,
  modas y medianas equivocados. Las pruebas de `integer64` corren todas con
  bit64 4.0.5 en R 4.2.0, y una prueba falla si alguna función de base que
  compara por debajo (`merge()`, `factor()`, `setdiff()`, `order()`…) recibe
  del paquete una columna `integer64`.

## Correcciones

- `planificar_limpieza()` no convierte un número escrito como texto que el
  perfil no lee: un teléfono como `099.123.456` entre importes con coma decimal,
  que el perfil deja fuera del resumen, hacía que el plan propusiera la
  conversión y `aplicar()` escribiera 99123456. Ahora la conversión queda
  `bloqueada`, como con cualquier valor que no responde al formato, hasta que se
  decida qué hacer con él. Con columnas iguales en las que algún nombre se
  repite en la tabla, el plan no propone acción —proponía dos `bloqueada` sin
  saber sobre cuál columna actuar— y el hallazgo queda en
  `hallazgos_sin_accion_por_columna_ambigua`, con el remedio.
- `inferir_tipo()` y `perfilar()` leen como número un importe con los miles
  agrupados (`1.234,56`, `1,234.56`, `12.500.000`) con la convención de la
  columna: quedaba `texto` y sin resumen, aunque la fila publicaba la
  convención, y la misma columna sin los puntos de miles se resumía. Si la
  columna aporta las dos convenciones, cada valor se lee con la que trae. Sin
  evidencia de convención —una columna de `1.234.567`— sigue como texto, y un
  primer grupo de miles que empieza con cero (`099.123.456`, la forma de un
  teléfono) no se lee como número en ninguna. El resumen lee sólo lo que la
  inferencia cuenta como número, también fuera de su muestra: lo demás —un
  teléfono, `0x1A`, `5.`— queda afuera y lo declara
  `n_valores_excluidos_resumen`. Un importe con moneda o unidad (`USD 1,234.56`,
  `12 kg`) sigue siendo un número escrito como texto: `numero_como_texto` lo
  señala y `planificar_limpieza()` lo convierte.
- La evidencia de `tipo_declarado_distinto` ya no dice que la conversión «no
  pierde ningún valor» cuando pierde algo: nombra el cero inicial de un código
  (`007`), la precisión, la convención sin decidir o las fechas de lectura
  ambigua, las mismas razones por las que el plan no convierte.
  `fecha_nacimiento_fuera_rango` y `fecha_fallecimiento_fuera_rango` cuentan las
  filas fuera de rango, que publicaban como `NA`, y su traza las nombra.
- `detectar_formatos_fecha()` reconoce la `t` y la `z` minúsculas de RFC 3339
  y cuenta la fecha civil distinta en UTC de un `POSIXlt` como la de su
  `POSIXct` (daba 0).
- `validar_ci_uy()` completa con ceros la cédula guardada como número (`1236`
  es `00001236`, y daba `FALSE`) y rechaza el cuerpo `0000000`, el relleno de
  muchas planillas. `validar_url()` rechaza la contrabarra y los invisibles de
  formato (U+200B, U+FEFF, U+200D, marcas de dirección): un escape de su clase
  de caracteres dejaba pasar `https://ejemplo.com\@otro.com`.
- Las relaciones aritméticas aditivas entre columnas con decimales fijos cuentan
  como rota una diferencia de medio cuanto o más mientras los valores tengan
  menos de 15 cifras significativas en ese cuanto: con centavos, importes de
  hasta 1e12; con enteros, hasta 1e14. Con la tolerancia relativa por omisión,
  `comision + neto = bruto` con 30 filas rotas por centavos salía `ok` con 0
  filas sobre importes de 1e8, y contaba 21 de 30 sobre importes de 1e6. Por
  encima de ese límite un decimal guardado no se distingue del redondeo de un
  texto de 15 cifras: la tolerancia relativa manda y la evidencia dice cuánto
  tolera. Con más de la mitad de las filas rotas por un cuanto la identidad no
  se publica como sana: se descarta, se cuenta en
  `meta$aritmetica_columnas$identidades_descartadas_cuanto` y deja una fila en
  `cobertura_diagnosticos` con la relación y el motivo. En una suma, la
  tolerancia se mide también sobre los sumandos: `a = 1e9 + x`, `b = -1e9 + y`,
  `c = x + y` acusaba 16 de 300 filas sanas. La evidencia declara la cota. Una
  columna que varía menos que la tolerancia relativa de su magnitud —los
  sumandos de `1e11 + x` y `-1e11 + y`— se evalúa con la cota del cuanto si sus
  tres columnas guardan decimales fijos; si no, `cobertura_diagnosticos` dice qué
  relación reconocía la tolerancia y por qué no se publicó (antes se descartaba
  sin rastro desde 1e11). Una suma con un total calculado en R cuyos sumandos se
  cancelan (`0.30 - 0.29` no es el doble de `0.01`) conserva sus centavos: sobre
  importes de 1e6 a 1e10 sus filas corridas un centavo salían `ok`.
- Las proporcionalidades entre importes con decimales fijos cuentan los
  centavos: `iva = round(neto * 0.22, 2)` con 30 filas corridas uno o treinta
  centavos salía `ok` con 0 filas desde netos de 1e9, y sobre netos de 1e7 un
  producto sano acusaba 8 filas. Con la tolerancia por omisión, si la tolerancia
  relativa supera la mitad del cuanto de la columna redondeada, esa mitad,
  inclusiva, es la tolerancia de todas las filas, y en `k` cada cifra de más
  tiene que explicar al menos una fila más: una constante larga no se traga las
  filas rotas de los importes grandes, y un tipo de cambio largo no se trunca
  para acusar los importes grandes sanos; una fila rota suelta no paga una
  cifra —en un empate, la más larga gana sólo si las filas que agrega son de
  las de mayor magnitud—, y entre dos del mismo largo gana la que deja más
  filas justo en el empate del redondeo (`0.0725` y no `0.0724` en
  `round(cant * 0.0725, 2)`). La evidencia publica también la mediana. La
  columna redondeada se lee como respuesta:
  `pesos = round(dolares * 39.8734, 2)` se publica
  `pesos ~= dolares * 39.8734`, y `resp ~= base * 1.22` no se lee al revés con
  `0.8196...`; si las dos lecturas caben y la tolerancia de una es el doble o
  más que la de la otra, se comparan con la fina —una fila que sólo cae
  adentro con la gruesa no cuenta, y las cifras de la constante se cuentan en
  la fina—. Con
  centavos llega a importes de 1e12, como la identidad. Un neto y un iva
  redondeados por separado cuentan como discrepantes las filas donde el iva no es
  el redondeo del neto guardado. La que la tolerancia relativa reconoce y
  ninguna resolución explica se descarta, se cuenta en
  `meta$aritmetica_columnas$proporcionalidades_descartadas_cuanto` y deja una
  fila en `cobertura_diagnosticos`; si la resolución de alguna de las dos
  columnas es compatible con la relación, se publica con la tolerancia
  relativa.
- El cuanto de una proporcionalidad es la resolución del redondeo que la
  columna muestra, no la potencia de diez de sus decimales: una respuesta
  redondeada a múltiplos de 0,05 —100 filas sanas con residuo de hasta 0,024—
  se descartaba porque la cota quedaba en medio centavo, y con 0,25, 0,5, 5 o 25
  no se publicaba nunca. La resolución se mide sobre los valores distintos y sin
  el cero: primero la de todos y, si con ella no alcanza el criterio, la de al
  menos el 90 % de las filas, y sólo si esa retícula no se explica por azar
  —contado en registros distintos: un registro repetido cuenta una vez, y cien
  importes que el redondeo junta en cinco valores cuentan cien—: unas filas
  rotas fuera de ella no la hacen caer —tampoco cuando son las únicas
  con más decimales que las demás—, y un valor que no es múltiplo de la
  resolución cuenta como discrepante aunque su residuo quede debajo de medio
  cuanto. Las identidades
  siguen con la potencia de diez de sus decimales, que entre valores de una
  misma retícula separa igual lo sano de lo roto.
- Un producto redondeado con respuestas chicas frente a su cuanto —un iva a
  centavos sobre netos de 10 a 1.000, o una respuesta a 0,05 de unos miles—, que
  la tolerancia relativa no reconocía, se lee con la mitad del cuanto si hay al
  menos 20 bases y 20 respuestas distintas y la relación explica más filas que la
  moda de la respuesta. Sin esas dos condiciones, entre columnas independientes
  con decimales fijos se publicaban proporcionalidades espurias (una binaria
  «era» la mitad de una columna casi siempre 1; una base estrecha «explicaba» la
  moda de la respuesta); con ellas, en las tablas de `datasets` y las del paquete
  no aparece ninguna relación nueva.
- Una proporcionalidad se sostiene en las filas que distinguen la constante:
  con `x` e `y` en cero en 180 de 200 filas y sin relación en las otras 20 se
  publicaba `x ~= y * 0.095` `sospechoso`, con las 20 filas que podían decir
  algo todas en contra; lo mismo con 180 filas en un mismo par de valores. Ahora
  tiene que alcanzar el criterio en las filas con base distinta de cero, donde
  el par más repetido pesa a lo sumo una de cada diez; una relación real con
  muchos ceros, como un iva sobre netos en cero, se sigue publicando, y una con
  más de una fila de cada diez rota entre las que no son cero no se publica. La
  que alcanza el criterio sin ese soporte se cuenta en
  `meta$aritmetica_columnas$proporcionalidades_descartadas_soporte`, también
  cuando una identidad entre las mismas columnas la vuelve redundante.
- Una relación cuya constante es la inversa de un número corto se lee con la
  división: `y = round(x / 7, 2)` se publica `y ~= x / 7` y no `y ~= x * 0.143`,
  que acusaba dos filas sanas, y `y = round(x / 3, 2)` con una sola fila grande,
  `y ~= x / 3` y no `0.3334`, que acusaba la grande sana o callaba la rota. La
  recíproca cuesta las cifras de su forma corta. Con las tolerancias de las dos
  lecturas parecidas, `y = round(x / 1.105)` en enteros se lee `y ~= x / 1.105`
  y no `x ~= y * 1.105`, que acusaba las filas sanas cuyo residuo cae entre las
  dos tolerancias (lo mismo con 1,09, 1,18 y 1,22), y `y = round(0.9 * x)` en
  enteros, `y ~= x * 0.9`: las filas que R redondea al par quedan en el medio
  cuanto y no se acusan. Una constante truncada que deja afuera filas sanas por
  un pelo ya no gana el empate a la entera: `y = round(x / 1.609344)` se lee con
  `0.62137` y no con `0.6214`, que acusaba dos sanas y callaba la rota, y
  `round(x / 0.9144, 2)` con `0.9144`. Con una tolerancia el doble de la otra,
  una fila corrida un cuanto ya no pasa por empate del redondeo: `y = x / 10` con
  tres filas corridas cinco centavos las acusa. En magnitudes angostas los datos
  pueden no distinguir una constante larga de una recíproca corta (`0.012345` de
  `1/81`, `0.393701` de `1/2.54`): gana la corta y puede acusar una fila sana.
- Una conversión que el plan no puede ejecutar dice cuántos valores no se leen:
  `n_no_reversibles` publicaba 0 al lado de un perfil que contaba los valores que
  la conversión dejaría en `NA` (un teléfono entre importes, `0x1A`, una columna
  `raw`), y su motivo cerraba con «se conserva como acción destructiva no
  recomendada» también sobre una acción `bloqueada` o no destructiva. Ahora
  termina con el estado: bloqueada, o lista sin activar hasta configurarla.
- `aplicar()` ya no aborta el plan entero cuando una acción de nombres no
  corresponde a los datos (un perfil con dos `x` y datos con una): la registra
  `fallida` con su motivo y sigue con las demás, como toda acción `lista` que
  falla; y el plan, con los datos, la bloquea antes. Una columna de nombre vacío
  que falta en los datos bloquea su grupo de columnas iguales. El lector de las
  columnas de un hallazgo usa la trazabilidad antes que el texto: una relación
  entre `a` y `b` ya no se confunde con una columna llamada `a,b`; un hallazgo
  de una columna sin nombre ya no se llama «de la tabla», y uno de varias
  columnas con una sin nombre va a la lista de columnas no identificables. Una
  acción cuyos parámetros no tienen la forma que espera —un vector, `NULL`—
  queda `fallida` con ese motivo, no con un error de R.
- Toda acción del plan sobre una columna que no está en los datos, y las de
  `columnas_duplicadas` cuando una columna del grupo no está en el perfil, quedan
  `bloqueada` con el motivo: con un perfil de otra versión de la tabla salían
  recomendadas y `aplicar()` las registraba `fallida`.
- El plan ya no declara un hallazgo de varias columnas —una relación aritmética
  en `neto,iva,total`, una de orden en `inicio,fin`— como de «columna ambigua»
  con el motivo «el nombre de columna se repite en la tabla», que era falso,
  también para las relaciones `ok`. Uno así va a `hallazgos_sin_accion` con su
  razón, nombrando sus columnas; un `ok` no entra a ninguna lista sin acción; uno
  de la tabla ya no habla de «esta columna»; y una columna que no está en el
  perfil lo dice en vez de decir que se repite.
- `detectar_claves()` con muchas copias de un identificador ya no arma las
  combinaciones que contienen una clave simple: con 100 copias tardaba 36 s y con
  1.000 `combn()` no entraba en memoria; ahora 0,2 s y 6 s, con la misma salida.
- `detectar_dependencias()` toma la muestra de una fila por tramo de la tabla,
  en una posición que sale de un hash determinista del tramo, y no a paso fijo:
  el paso fijo se alineaba con un panel mensual y tomaba sólo dos meses de cada
  doce. Sigue siendo la misma en cada corrida, sin azar.
- `desviacion_benford` dice si la desviación la producen los faltantes
  disfrazados que no se declararon, con la prueba sin ellos: 200 valores `999`
  entre 4.000 importes daban p = 1e-45, y sin ellos p = 0,22.
- Con `sentinelas_numericos` declarado, `valor_concentrado` deja afuera lo
  declarado, como el resumen: publicaba como moda el `-99` que el resumen
  excluía.
- `detectar_dependencias()` no publica un par cuyo determinante no acierta más
  filas que la moda del dependiente (con un `estado` al 99,66 % salían cinco
  dependencias que no decían nada), y lo que encuentra en la muestra lo
  recalcula sobre la tabla completa: una muestra alineada con la estructura de
  los datos publicaba 0,996 de cumplimiento donde la tabla da 0,899.
  `detectar_relaciones()` calcula las coberturas sobre las columnas completas
  (con una muestra alineada publicaba 1 donde era 0,5) y `muestra` ya no
  recorta. `clasificar_variables()` cuenta `n_niveles_observados` sobre la
  columna entera también sin perfil, y `meta$aritmetica_columnas` se publica
  también cuando la única identidad candidata se descarta.
- La identidad de filas ya no desborda en R anterior a 4.6. Con dos o más
  columnas de decenas de miles de valores distintos, `aplicar()` con
  `conservar_primera_duplicada` podía borrar filas legítimas (23.673 de 60.100
  en una tabla con 200 repetidas), y `EntidadDuplicada`, `referencial()` y las
  acciones de duplicados contaban grupos equivocados. Una matriz o una tabla
  anidada como columna se aplana para esa identidad, como en el perfil.
- `medir()` ya no se cae con una columna lista —lo que un motor devuelve para
  una columna binaria— cuando una métrica no puede producir medidas: la métrica
  queda `no_medible`. Una tabla anidada, una matriz de listas o un entorno en una
  celda tampoco la hacen caer, y `perfilar()` ya no publica más filas completas
  que filas con una matriz de listas.
- `NoNulo` compara `valores_nulos` con la misma normalización que `perfilar()`:
  sin los blancos de los bordes y sin distinguir mayúsculas. Una celda de solo
  blancos o un tabulador cuentan como nulos para los dos.
- `agregar()` de `instanciaEntidad` a `entidad` conserva la instancia en el
  nombre (`agregada:ratio:ClaveDuplicada@t.a.b`): dos claves compuestas de la
  misma tabla ya no se funden en una sola fila con un resultado mezclado. De
  `instanciaAtributo` a `atributo`, dos instancias de una métrica sobre la misma
  columna llevan la instancia en el nombre solo en ese caso. Más arriba, el
  nombre conserva la instancia que traen todas las partes, y `agregar()` se
  niega a promediar dos instancias de una misma tabla o columna, que la harían
  pesar doble, y dice cómo seguir.
- `medir()` arma su tabla de medidas de una sola vez, sin una tabla intermedia
  por métrica: sobre una tabla de 80.764 filas y 153 columnas de texto con 100
  métricas, bajó de 133 s a 56 s y su pico de memoria de 4,9 GB a 3,5 GB, que es
  del orden de la tabla de medidas que devuelve (8 millones de filas).
- En `perfilar_dbi()` con `universo = "tabla_completa"`, el desvío de las
  columnas de SQL Server y de `double precision` de PostgreSQL se calcula en
  lotes, centrado en la media que ya trajeron los básicos, y no en una consulta
  por columna. Sobre una tabla de un millón de filas y 27 columnas numéricas,
  el desvío pasó de 27 consultas a 2 y de 1,42 millones de lecturas lógicas a
  105 mil en SQL Server, con las mismas cifras. En un motor que lee del disco,
  cada consulta de desvío recorría la tabla entera.
- El desvío de una columna cuyo mayor valor absoluto queda fuera de
  `[1e-150, 1e150]` se calcula sobre sus valores divididos por una potencia de
  dos y centrados en su propia media, en los cinco motores y en `perfilar()`.
  Cerca del máximo del doble (±1,8e308), SQL Server, MySQL y MariaDB dejaban el
  desvío en `NA` sin avisar y `perfilar()` publicaba `Inf`; con valores del
  orden de `1e-160` todos perdían cifras (error de hasta 7e-4), con `1e-200`
  `perfilar()` publicaba `0`, y en PostgreSQL 16, por encima de `1e150`, el
  desvío pasaba por un `numeric` de 15 cifras (error de 4e-5). Ahora queda a
  menos de 1e-14 de la cifra exacta, o `no_disponible` con su falla y el aviso.
  Las columnas de magnitud común conservan la consulta y la cifra.
- En MySQL y MariaDB, el desvío de una columna con media entre `1e65` y `1e150`
  perdía cifras (error de hasta 9e-5): el centro se escribía como un decimal de
  más de 65 cifras. Y cuando la suma desbordaba, MySQL publicaba la media en `0`
  y MariaDB en `NA`, sin avisar; ahora la media también se calcula escalada.
- Con `RMySQL`, un perfil ya no suelta decenas de avisos del controlador
  (`Decimal MySQL column imported as numeric`): lupa pide en el SQL el tipo que
  lee, con las mismas cifras. Un `error while fetching rows` del controlador
  cuenta como falla de la consulta y no como un resultado vacío.
- En MySQL y MariaDB, un `DECIMAL` con valores de 2^53 o más ya no publica
  mínimo, máximo, media, mediana ni desvío con cifras falsas
  (`12345678901234567168` por `12345678901234567890`): quedan `no_disponible`
  con su motivo, como en PostgreSQL y SQL Server. Con `metricas` explícitas, ese
  motivo decía «no se pidió».
- La moda de una columna binaria se publica como su largo y su MD5
  (`400:63f9…`), la misma representación que usa la muestra, en minúsculas en
  todos los motores; la corroboración ya no la marca `no_corroborada`, y en el
  perfil de la muestra la columna figura como `blob`. Con
  `universo = "muestra_motor"`, una columna `char(n)` de SQL Server ya no
  publica la moda con los espacios de relleno.
- Una muestra que el tope de bytes deja en cero filas publica
  `filas_obtenidas = 0` y el aviso dice cero filas, no `NA`. Con `TABLESAMPLE`
  o `RANDOM()`, `meta$orden_muestra` declara que `orden_muestra` no gobierna la
  selección.
- Una moda de frecuencia 1 sobre dos o más valores ya no se publica cuando
  `n_distintos` no se midió —no se pidió, o su consulta se cortó por tiempo—:
  ningún valor se repite.
- Con `universo = "muestra_motor"`, un `max_consultas` chico ya no hace
  abortar el perfil. Con valores de 1 a 12, según el motor, la corrida terminaba
  en error: el presupuesto se agotaba al leer el esquema, o al contar la tabla
  que `TABLESAMPLE` necesita para escribir su porcentaje. Ahora degrada y lo
  declara en `meta$consultas$agotado`, con las métricas que no entraron como no
  calculadas, la cobertura y el aviso.
- Con un tope de bytes bajo, la muestra ya no queda vacía por una cota que no
  es la real: para `text` o `bytea` sin largo declarado se estimaban unos 125 KB
  por fila, y con `max_bytes_muestra = 100000` sobre filas de 3,7 KB llegaba
  una. Ahora se lee una fila, se mide lo que ocupa y se sigue en bloques sin
  pasar el tope —en ese caso, unas 25 filas—, y `meta$muestreo$filas_por_bytes_fuente`
  lo declara.
- Con `universo = "muestra_motor"`, el desvío se pide centrado en la media de
  las mismas filas y en la misma consulta: una relación derivada sobre la
  muestra calcula esa media con una ventana (`AVG(x) OVER ()`). Sin centrar, en
  SQL Server unos montos de un millón daban el desvío con un error de 2,3e-3, y
  en PostgreSQL 16 valores casi constantes, de 5e-3; en MySQL y MariaDB pasaba
  también con `metricas = "desvio"` en `tabla_completa`. Esa misma evaluación de
  la muestra da las filas obtenidas, decide el reintento y entra en los
  agregados: con `TABLESAMPLE SYSTEM`, algunas corridas dejaban la media y el
  desvío no disponibles mientras declaraban 300 filas obtenidas.
- El desvío extremo se escala aunque no se pidan los básicos ni se publiquen
  los valores (`incluir_valores = FALSE`), y también cuando el rango es tan
  chico que sus cuadrados caerían en subnormales. Antes MariaDB llegaba a
  publicar `1.7976931348623157e+308` como desvío y MySQL una media de 0. Con un
  solo valor válido, el desvío es `no_aplica` y no dispara un repliegue.
- En SQL Server, la media de una columna cuya suma desborda, y en los cinco
  motores la mediana de dos centrales enormes, se calculan escaladas en lugar
  de quedar no disponibles; MySQL publicaba esa mediana en 0.
- Con `bloque_filas`, el desvío y la media de valores extremos ya no salen
  `Inf`, `0` ni `NA` como calculados: el estado se reescala por una potencia de dos
  cuando llega un bloque que la escala vigente no cubre. La moda de una columna
  binaria también sale en esa vía, como largo y MD5, y una columna `image` de
  SQL Server publica su moda.
- Sin el paquete opcional `stringdist`, el diagnóstico de proximidad del
  vocabulario deja una sola fila de cobertura por columna, la que dice que falta
  el paquete. Antes agregaba otra que hablaba de un presupuesto agotado («se
  comparó entre sí a las 0 primeras») y recomendaba subir
  `max_trabajo_vocabulario`, que no arreglaba nada.
- Con `bloque_filas` y un controlador sin lectura incremental demostrada
  —DuckDB, o uno que no está en la matriz—, el perfil sigue sin caer en
  `dbGetQuery()`, pero ahora lo avisa (`lupa_aviso_muestra_motor`): antes todas
  las métricas quedaban `no_disponible` y el motivo solo estaba en la cobertura.
- Con `universo = "muestra_motor"`, `estado` y `error_esperado` se deciden por
  métrica y con la misma regla en las dos vías, como dice la ayuda. Antes, con
  `bloque_muestra = "solo_agregados"`, los distintos y la mediana de la muestra
  salían `error_esperado = "no_aplica"`, lo que se publica cuando no hubo
  muestreo; y con `"con_muestra"` todas las métricas salían `no_estimable` —también
  la media y los conteos— y los estadísticos `observado_muestra` en vez de
  `estimado`. Los valores no cambian.
- Los mensajes, avisos, evidencias e informes concuerdan en número («1 fila»,
  «1 valor») en todo el paquete, llevan sus tildes y dan las indicaciones en forma
  impersonal («revisar», «aumentar») en lugar de «usted». Los valores que el
  paquete publica como dato —estados, motivos codificados, unidades, las marcas
  de protección y las clases de consulta del plan— no cambian.
- `n_filas()` de una colección devuelve el vector con el nombre de cada tabla.
- El aviso de tabla ancha proyecta con 5.000 celdas por segundo, por debajo de
  todas las mediciones de `benchmark/medir_referencias.R`; los 10.000 de antes
  eran más rápidos que lo medido (unas 5.200 con 50 columnas), así que la espera
  anunciada se quedaba corta. `meta$costo_tabla_ancha$fuente` nombra los 2
  guiones que la sostienen: ese y `benchmark/medir_topes_valores_ancho.R`, con
  columnas iguales de códigos de texto.
- La ayuda de `estrategia` en `detectar_duplicados_aproximados()` dice
  exactamente cuándo `"auto"` pasa a LSH (más de 10.000 filas, `muestra = Inf` y
  `max_pares` de 50.000.000 o más), y el aviso de LSH ya no llama «piso» a una
  referencia de la comparación aislada.
- Con `bloque_filas`, las columnas binarias se leen como largo y MD5, igual
  que en la muestra: con RMySQL el controlador ya no las trunca (un aviso por
  fila, y largos y distintos falsos), y en PostgreSQL, SQL Server y MariaDB los
  distintos y los largos dejan de quedar no disponibles.
- La moda de un entero de 2^53 o más que el controlador entrega como doble —en
  DuckDB `BIGINT`, `UBIGINT`, `HUGEINT` y `UHUGEINT`, y con RMySQL— ya no sale
  con cifras falsas (`1234567890123456768` por `1234567890123456789`): se pide
  como texto exacto, también con `bloque_filas`. En DuckDB, un `UHUGEINT` de esa
  magnitud ya no publica mínimo, máximo, media ni mediana falsos, y la mediana
  de un `HUGEINT` cerca de su tope, cuya consulta de básicos desborda en el
  motor, queda no disponible en vez de publicarse como un doble.
- Con `bloque_filas`, una columna de un solo valor o vacía ya no publica `NA`
  como calculado: el desvío queda `no_aplica` y las métricas de una columna
  vacía, `sin_valores`. Y con RMariaDB un `bigint unsigned` mayor que 2^63, que
  el controlador entrega dado vuelta a negativo, ya no se publica como
  calculado: queda no disponible con su motivo.
- En MariaDB, la mediana de valores extremos ya no sale `100000000` o `0` como
  calculada.
- La moda de un decimal de 2^53 o más ya no sale con cifras falsas
  (`12345678901234567168` por `12345678901234567890`): se le pide al motor como
  texto exacto en PostgreSQL, SQL Server, MySQL, MariaDB y DuckDB. Con
  `incluir_valores = FALSE`, el motivo de precisión de esas columnas ya no se
  reemplaza por uno de privacidad.
- En PostgreSQL, un `NaN` en la columna ya no deja el mínimo, el máximo, la
  media y el desvío sin calcular: se miden sobre los finitos, como en
  `perfilar()`. Con el presupuesto agotado, `n` queda `no_disponible` con su
  motivo y no `NA` como calculado.
- En R anterior a 4.6, `match()` sobre fechas era cientos de veces más lento, y
  una columna `Date` hacía tardar minutos el perfil: 100.000 fechas pasaron de
  171 s a 1,3 s en R 4.5.2. `Date`, `POSIXct`, `difftime`, los factores e
  `integer64` se comparan por una clave sin clase, con el mismo resultado.
- Los valores de texto de más de `max_largo_valor_vocabulario` (10.000
  caracteres o bytes) quedan fuera solo de las operaciones textuales caras
  —normalización, patrones, reconocedores de datos personales, casi
  duplicados—, y la cobertura lo declara con `texto_largo_no_evaluado` y una
  fila por diagnóstico afectado, para que `comparar_perfiles()` diga
  `no_evaluado` y no `resuelto`. Los conteos, la inferencia de tipo, los
  blancos, los centinelas y las dependencias siguen leyendo el valor entero.
  Sobre 700 filas de cuatro columnas de 14 KB, el perfil pasó de 23 s a 5 s.
- Un nombre de columna con coma ya no rompe la traza: los hallazgos de pares de
  columnas llevan las columnas como vector en `trazabilidad$columnas`, y la
  cobertura en una columna `columnas`. Una clave con un nombre así se pide con un
  vector.
- `cobertura_analisis()` acepta el resultado de `perfilar_dbi()`: analiza la
  muestra y dice que la cobertura es de la muestra; con `bloque_muestra =
  "solo_agregados"`, usa el resumen de la tabla.
- Con `universo = "muestra_motor"`, los topes de bytes y de celdas
  (`max_bytes_materializacion`, `max_bytes_muestra`, `max_celdas_muestra`)
  mandan antes de leer. Cada `dbFetch()` se pide con un tamaño máximo de fila
  conocido de antemano: los textos de más de `max_largo_valor_vocabulario` y las
  columnas binarias (`bytea`, `varbinary`, `image`, `blob`) se piden como largo
  y resumen MD5, con los distintos, los duplicados, los faltantes y los largos
  exactos. Si el presupuesto se agota, se conserva lo que entró. Antes, una
  columna binaria o de textos largos podía traer varios GB a R antes de mirar
  el tope.
- Una muestra del motor que vuelve corta se reintenta una vez, con `BERNOULLI`
  en PostgreSQL o una fracción mayor por bloques, sin ordenar la tabla entera;
  el aviso sale solo si la muestra final sigue corta o la recortó un tope.
- Las consultas pedidas que el motor rechaza o cancela quedan en
  `resumen_tabla$meta$consultas$fallas` y en la cobertura como `degradado`, con
  un aviso de clase `lupa_aviso_muestra_motor` que nombra ese camino completo
  (antes decía `meta$consultas$fallas`, que sobre el resultado da `NULL`); las
  consultas internas del paquete (capacidades, catálogo, configuración) no
  avisan. La muestra del motor publica
  también sus tiempos, sus bytes y el tope que mandó.
- Por JDBC, SQL Server se reconoce como motor por los metadatos de la conexión.
- `perfilar_dbi()` avisa (`lupa_aviso_costo_dbi`) antes de recorrer con
  agregados una tabla que el catálogo estima en 100 millones de filas o más, y
  sugiere `universo = "muestra_motor"`.
- La mediana de MariaDB ya no sale de `PERCENTILE_CONT`, que ese motor devuelve
  con 10 decimales y un tope en 1e8: con `perfilar_dbi()` por omisión, una
  columna de montos del orden de 1e12 publicaba 1e8, una de valores del orden de
  1e-12 publicaba 0 y un doble común perdía desde el décimo decimal, todo como
  `calculado`. La prueba de capacidad del percentil exige ahora que vuelvan todas
  las cifras, y MariaDB calcula la mediana por columna (tabla completa) o con la
  CTE de ventanas (muestra), como MySQL. Vale también con
  `estrategia_mediana = "aproximada_motor"` y con RMySQL contra un servidor
  MariaDB.
- Con `universo = "muestra_motor"` y `bloque_muestra = "solo_agregados"`, la
  mediana sale en MySQL, en un `float8` de PostgreSQL y en una columna con
  valores extremos de cualquier motor: la calcula la CTE de ventanas sobre la
  misma muestra, como con `con_muestra`. La consulta de prueba de esa CTE usaba
  una construcción que MySQL rechaza.
- Con `metricas` sin la cuenta de valores válidos, la mediana de la tabla
  completa cuenta esos valores en una consulta propia y sale igual que con todas
  las métricas. Con `dialecto = "portable"`, que no tiene salto de filas, sale
  de la CTE de ventanas si el motor la acepta.
- Ningún registro de `perfilar_dbi()` afirma un valor ausente: un estado
  `calculado`, `estimado` u `observado_muestra` con el valor en `NA` pasa a decir
  por qué falta. Es `sin_valores` si la columna está vacía (en la muestra,
  `no_disponible` con `muestra_inestable:sin_valores_validos`), y `no_aplica`
  para una métrica numérica sobre una columna que no lo es, para el desvío de
  menos de 2 valores y para la moda de una columna sin repetidos (en la muestra,
  `no_disponible`, porque la tabla puede tener moda).
- En SQL Server, la mediana de un `decimal` con 36 cifras de precisión o más
  ya no pierde decimales: `AVG(valor * 1.0)` recortaba la escala a 6, y en un
  `decimal(38,7)` la mediana de 0,0000001 y 0,0000003 salía 0. Con
  compatibilidad menor que 110, sin `PERCENTILE_CONT`, ese era el camino de todo
  `decimal`. Ahora el motor suma las una o dos centrales en el tipo de la
  columna, y R divide esa suma por cuántas son: la mediana es el doble más
  cercano de la exacta. Convertirlas a doble en el servidor la dejaba a un ulp,
  y promediar en R el doble de cada central tampoco es exacto (la de 0,10 y
  0,20 daba 0,15000000000000002). Lo mismo en MySQL y MariaDB, donde
  `AVG(valor * 1.0)` necesita una cifra de escala más que la columna y en la
  escala máxima del motor (30 y 38) se recortaba: la mediana de 1e-30 y 2e-30
  salía 2e-30.
- En PostgreSQL 9.3 con `dialecto = "portable"`, y en la muestra con la
  subconsulta escalar, la mediana de una cantidad par de `float8` coincide con
  `median()`: difería en 1 ulp en 427 de 3.000 pares. Usa el mismo promedio que
  la vía de dos consultas.
- En `perfilar()`, una errata de texto (`Montevideo`/`Montevido`) ya no queda
  callada porque la misma columna tenga una familia de códigos numerados con
  frecuencias desparejas. La familia sigue declarando que el diagnóstico no
  aplica a las formas con números; los grupos sin números se juzgan con el mismo
  límite y contra el mismo vocabulario entero de la columna: salen igual que si
  la familia no se encadenara.
- La búsqueda de casi duplicados del vocabulario ya no depende de que la
  columna tenga, antes de una pareja, dos formas que se normalizan igual (`ALFA`
  y `Alfa`). Con una de esas clases delante, el filtro de marcas de formato leía
  otro par: en una columna de horas publicaba como casi duplicados pares
  `a.m.`/`p.m.` que tiene que descartar y perdía el hallazgo real; la fila de
  cobertura «N grupos de formas cercanas no se formaron…» desaparecía, y el
  diagnóstico de variantes equifrecuentes nombraba formas que no son pareja. Sin
  esas clases, o con la clase después de la pareja, la salida es la misma.
- En `perfilar()`, una pareja cercana cuyas formas normalizadas empatan en
  frecuencia ya no queda sin declarar porque uno de sus lados junte grafías que
  se normalizan igual (`Montevideo` y `MONTEVIDEO`, 25 cada una, contra
  `Montevido`, 50). Se publicaba sólo el grupo de esas grafías, y la pareja no
  aparecía en la fila de cobertura «N grupos de formas cercanas no se
  formaron…» ni, con `variantes_equifrecuentes_vocabulario = TRUE`, como
  hallazgo. Ahora se declara igual que sin esas grafías, y el grupo
  equifrecuente las lista todas con su frecuencia; su asimetría es la de las
  formas normalizadas (`MONTEVIDEO` 10 y `Montevideo` 40 contra `Montevido` 50
  da 1, no 5).
- En `perfilar()`, una pareja que `casi_duplicados_vocabulario` ya publica no se
  cuenta además en la fila de cobertura «N grupos de formas cercanas no se
  formaron…», ni sale otra vez como `variantes_equifrecuentes_vocabulario` con
  el detector encendido. Con los valores por omisión pasaba en el borde:
  `Montevideo` (50) contra `Montevido` (25) tiene asimetría 2, que llega al piso
  de `min_asimetria_vocabulario` y no pasa el tope de
  `max_asimetria_equifrecuente_vocabulario`, y la salida publicaba la pareja y
  decía a la vez que no se había formado. Lo mismo con las variantes de igual
  frecuencia de un grupo publicado (`Zona B` … `Zona O` alrededor de `Zona A`).
  Lo que el piso de asimetría deja afuera se sigue contando. Y la cantidad de
  pares que esa fila declara («Se identificaron M pares cercanos…») es la de los
  grupos que cuenta: sumaba también los de los grupos ya publicados y los de las
  cadenas que el tope de asimetría o el límite del vocabulario dejan afuera.
- En `perfilar()`, cada par cercano del vocabulario se declara una sola vez
  también cuando el grupo de formas cercanas toca, por otra de sus formas, un
  grupo que `casi_duplicados_vocabulario` publica. Con `Montevideo` (100),
  `Montevido` (50) y `Montividoo` (50) se publicaba [Montevideo / Montevido] y
  la fila de cobertura decía a la vez «1 grupo de formas cercanas no se formó…
  Se identificaron 2 pares», contando esa misma pareja; con
  `variantes_equifrecuentes_vocabulario = TRUE` la pareja salía otra vez dentro
  de [Montevideo / Montevido / Montividoo]. Ahora la fila cuenta 1 par y el
  grupo equifrecuente es [Montevido / Montividoo]: la forma que comparten se
  nombra en los dos grupos y cada uno declara sus pares. Un par que quedaba sin
  declarar porque la pareja publicada llevaba la asimetría del grupo por encima
  de `max_asimetria_equifrecuente_vocabulario` ahora se cuenta, y dos grupos que
  sólo se tocaban por una pareja publicada salen separados. Los grupos de
  `casi_duplicados_vocabulario` no cambian.
- En la fila de cobertura de `proximidad_vocabulario`, los pares cercanos que
  forman cadenas cuya asimetría de punta a punta supera
  `max_asimetria_equifrecuente_vocabulario` se declaran aparte: con
  `Montevideo` (25), `Montevido` (20) y `Montividoo` (10), a 1,25 y 2 de a
  pares y a 2,5 de punta a punta, la fila dice «…2 pares cercanos, con
  asimetría <= 2.0 entre ellos, forman cadenas cuya asimetría de punta a punta
  supera 2.0 y no se agrupan». Antes no se declaraban en ningún lado. El tope
  por grupo no cambia.
- La mediana de una columna entera o decimal con valores de 2^53 o más se decide
  por su valor y no por la columna, con la misma regla en los siete motores: con
  una cantidad impar se publica si la central queda por debajo de 2^53 (la de
  `{1, 2, 2^63 - 1}` en `BIGINT` es 2, y quedaba `no_disponible`); con una par,
  si las dos centrales quedan entre -2^52 y 2^52, lo que en la tabla completa
  cuesta una consulta más. En DuckDB, la de un `HUGEINT` cerca de su tope con una
  cantidad par ya no sale del promedio de dos dobles redondeados (2,5 por 2).
  La misma regla siguen `perfilar()` sobre `integer64` o sobre enteros escritos
  como texto, `perfil_muestra` y la muestra del motor, que tienen las centrales
  a la vista: la corroboración ya no acusa una divergencia entre el resumen SQL,
  que publicaba la mediana, y la muestra, que la negaba por la columna.
- En DuckDB, la mediana de un `DECIMAL` ya no se trunca a la escala de la
  columna: la de 1,00 y 1,01 en `DECIMAL(10,2)` salía 1,00.
- En PostgreSQL, la mediana de una cantidad par cuyas centrales distan más de
  2^53 ya no se publica con el redondeo de `PERCENTILE_CONT` (-1 por -0,5).
- Con `bloque_filas` en SQLite, una columna `INTEGER` cuyos primeros bloques
  entran en 32 bits y otros no ya no cuenta mal `n_distintos`, la moda ni la
  mediana (`{1, 2, 3e9, 3e9}` daba 4 distintos, ninguna moda y mediana 0,5).
- En SQL Server con compatibilidad 110 o más, el desvío de un `decimal(38, s)`
  ya no sale mal cuando la media termina en la mitad de la última unidad:
  `{1, 2, 3, 4}` en `decimal(38,0)` daba 1,8257 por 1,2910.
- Con `bloque_filas`, y por la muestra del motor, el desvío de un decimal que el
  doble no representa ya no sale de esos dobles: un `numeric(18,2)` de 5e14 con
  desvío 3 daba 3,1115 por 3,1047, y con la muestra leída en R
  (`bloque_muestra = "con_muestra"`) se publicaba movido en una parte en 300.
  Por la tabla completa y por bloques, el motor suma en el tipo de la columna,
  y devuelve como texto, la columna y los cuadrados de sus diferencias con la
  media escrita en su escala, y R saca el desvío exacto: es el doble más
  cercano. Con la muestra leída en R sale del texto de cada valor, que la
  muestra ya traía. Por la muestra del
  motor con `bloque_muestra = "solo_agregados"` sale de las mismas sumas,
  pedidas en la sentencia de los agregados. Un decimal de escala 0
  con valores por debajo de 2^53 se publica en todas las vías: ahí los dobles
  son los valores, y la escala la dicen los catálogos de PostgreSQL, MySQL,
  MariaDB, DuckDB y SQL Server, que ahora publica la precisión y la escala de un
  decimal (`decimal(18,0)` y no `decimal`).
- Con `bloque_filas` y con la muestra leída en R, la media y la mediana de un
  decimal que el controlador entrega como doble son el doble más cercano de las
  del decimal, y no las de esos dobles: la de 0,0001 y 0,0002 daba
  0,00015000000000000001, a un ulp en 14 de 112 medias y 9 de 112 medianas de
  SQL Server y en 4 y 2 de 46 de PostgreSQL, MySQL, MariaDB y DuckDB, y con
  valores de hasta 1e12 con seis decimales que se cancelan, una media por
  bloques de `numeric(20,6)` en PostgreSQL quedaba a 1.447 ulps. Por bloques el motor suma la columna en su tipo, en una consulta para
  todas las columnas decimales, y devuelve como texto las dos centrales cuando
  caen en dobles distintos (una consulta ordenada por columna; `money` de SQL
  Server siempre, porque odbc no lo lee al doble más cercano). La muestra leída
  en R ya traía cada decimal como texto.
- La media y el desvío de un decimal por la tabla completa son los dobles más
  cercanos de los exactos. MySQL, MariaDB y SQL Server promediaban el doble de
  cada valor, y con valores que se cancelan la media salía lejos:
  `999999999999999.99`, `-999999999999999.98`, `0.01`, `0.02`, `0.03` y
  `-0.04` daban 0,00333 por 0,005; y el desvío de los cinco motores con
  decimales, y la media con escala 30, quedaban a un ulp. El motor suma la
  columna y sus cuadrados en su tipo y los devuelve como texto, en la misma
  consulta que los demás agregados, y R divide y saca la raíz con aritmética
  entera. Por bloques, el desvío de un decimal sale de una consulta de sumas,
  sin consultas de más: el de 500000000000006.60 y sus
  vecinos quedaba a un ulp, y el de `1.10`, `2.20` y `3.30`, también. Con
  `incluir_valores = FALSE`, el SQL publicado lleva la media tapada.
- Las sumas exactas de los decimales van por lotes de `tamano_lote_planos`
  columnas —por la tabla completa y la muestra del motor, los de los agregados;
  por bloques, los suyos, con las expresiones que acepta cada motor— y cada
  expresión se elige para que no desborde ni redondee con las cifras del tipo y
  las filas. Con una
  sola consulta para toda la tabla, 600 columnas `numeric(18,2)` pasaban el
  límite de 1.664 expresiones de PostgreSQL y por bloques no quedaba ninguna
  media; en DuckDB el cuadrado de un `DECIMAL(15,0)` desbordaba en 64 bits y
  todas las columnas de la tabla quedaban con la media y el desvío del motor; y
  un `decimal(38,30)` de SQL Server con valores de -1e8 a 1e8 no tenía cuadrado
  exacto y quedaba el `STDEV` del motor, a uno o dos ulps. Un lote que el motor
  rechaza se parte hasta aislar la columna, y el plan cuenta los lotes y, como
  máximo, los reintentos. Las sumas salen ahora con cualquier `metricas` (con
  `c("basicos", "desvio")` o sólo `"desvio"` la tabla completa publicaba la del
  motor: en PostgreSQL 9.3, 1,63 por 0,1425), con `incluir_valores = FALSE`, con
  un `max_consultas` igual al total del plan, con valores de 2^53 o más —cuya
  media y desvío quedaban `no_disponible`— y bajo la muestra del motor con
  `bloque_muestra = "solo_agregados"`, en la misma sentencia que los demás
  agregados (la media de un `DECIMAL(18,2)` cambiaba con el orden de las filas).
  En DuckDB la media y el desvío del motor de esas columnas se piden sobre el
  doble: `x * 1.0` de un `DECIMAL(38,30)` de 9e7 desbordaba y tumbaba la
  consulta de los agregados, con las sumas adentro.
  Por bloques, `NaN` e `Infinity` de PostgreSQL quedan fuera de las sumas, como
  en la tabla completa. Si las sumas no llegan, la media y el desvío del motor
  por la tabla completa quedan sólo si su error cabe en una parte en mil
  millones; si no, `no_disponible`, con el motivo de por qué no llegaron: el
  motor rechazó la consulta con las sumas, con su mensaje, o no hubo
  presupuesto. Si llegan sin valores, el estado sale de la cuenta: una columna
  toda `NULL` queda `sin_valores` y el desvío de una sola fila, `no_aplica`. La escala de un decimal de una tabla
  temporal la leen el catálogo de `tempdb` en SQL Server y `SHOW COLUMNS` en
  MySQL y MariaDB: sin ella, el desvío quedaba a un ulp o `no_disponible`.
- Sin `validos` en `metricas`, el mínimo, el máximo, la media y el desvío de un
  doble o un entero no sabían cuántos valores vieron: una columna toda `NULL`
  daba la media `no_disponible` con «La consulta terminó sin un valor para la
  métrica» y el desvío `no_aplica`, y el desvío de una sola fila
  `no_disponible`, donde la ayuda promete `sin_valores` y `no_aplica`. La cuenta
  de los no nulos va ahora en la misma consulta de los agregados, sin consulta
  nueva, y decide como la de las sumas exactas de un decimal. Por el spool, una
  tabla sin filas publicaba `n_validos`, `n_faltantes` y `n_distintos` en cero
  como `observado_muestra`; ahora, como una muestra vacía de una tabla con
  filas, `no_disponible` con `muestra_vacia:<método>`.
- La cuenta que decide si hay valores es la de los valores finitos, los que usan
  las métricas de magnitud. Sin `validos` en `metricas`, una columna de
  PostgreSQL con `(NaN, NULL)` publicaba el desvío `no_aplica` mientras el
  mínimo, el máximo y la media decían `sin_valores`, y un `numeric` sólo `NaN`
  decía «La columna no contiene valores no nulos», que es falso. Ahora la
  columna que mostró `NaN` o infinitos la decide la vía de los valores finitos,
  con su motivo, y si la magnitud no se pudo establecer queda declarada con ese
  motivo.
- Una columna `real` de PostgreSQL con `NaN` o infinitos ya no deja una sentencia
  girando: la expresión exacta de los valores finitos se multiplicaba al fundirse
  las subconsultas, y el planificador tardaba minutos aun sobre tres filas, sin
  atender a `statement_timeout`.
- Con `universo = "muestra_motor"` y `bloque_muestra = "solo_agregados"`, cada
  sentencia sortea su muestra, y lo que decidía los agregados de una columna
  podía salir de otra. La sonda de magnitud era una consulta propia: si le tocaba
  una muestra vacía, el desvío se publicaba `no_aplica` con un lote de cien
  valores. En SQL Server, con `metricas = "desvio"`, cada columna de coma
  flotante forzaba un repliegue que sorteaba otra muestra: publicaba el desvío de
  filas que la corrida no publica, o `no_disponible`. Ahora la magnitud viaja en
  el lote de agregados, la misma sentencia que el desvío, y
  `plan_perfilado_dbi()` ya no la cuenta aparte. Bajo la muestra no hay repliegue
  escalado de la media ni del desvío: una columna con valores de más de 1e150 o
  de menos de 1e-150 en valor absoluto queda `no_disponible` con su motivo, en
  lugar de publicar el valor de otra muestra.
- En MySQL y MariaDB, con `metricas = "desvio"`, una tabla sin filas por la
  muestra del motor publicaba el desvío `no_aplica` («requiere al menos dos
  valores»): al pedirse el desvío solo, la consulta perdía el total del lote.
  Ahora queda `no_disponible` con `muestra_vacia:<método>`, como en los demás
  motores, y `meta$filas_obtenidas_muestra` dice 0.
- Con `incluir_valores = FALSE`, el SQL publicado del desvío centrado de un
  entero o un doble lleva la media tapada: publicaba la media como literal
  (`STDDEV_SAMP((b - 731946280004.333) * 1.0)`), en los siete motores.
- `plan_perfilado_dbi()` cuenta lo que la corrida emite también sin `validos`:
  los agregados llevan el `COUNT(*)` del total, que se pedía aparte y el plan no
  contaba (13 contra 14 consultas con `metricas = c("basicos", "desvio")` en
  PostgreSQL), y las sondas de magnitud se cuentan con la regla de la corrida,
  que no las pide para un entero chico con sólo `desvio` (dos de más por tabla).
- `plan_perfilado_dbi()` cuenta la consulta de prueba de la CTE de ventanas, y
  su control negativo, que la muestra del motor corre cuando una columna
  decimal, o un `float8` de PostgreSQL, deja la mediana consolidada: dos
  consultas que el plan no anticipaba.
- `plan_perfilado_dbi()` anuncia lo que `perfilar_dbi()` manda al motor en cada
  vía, contado sentencia por sentencia en los siete motores. Por `bloque_filas`
  contaba sólo el recorrido —1 consulta donde se mandaban de 8 a 24— y ahora
  cuenta la preparación, las consultas de prueba de la fuente, la segunda pasada
  del localizador, la moda exacta, la media de los decimales y la muestra, con la
  mediana por centrales y el desvío centrado de un decimal en `total_maximo`,
  porque los piden los datos. Por la muestra del motor sin bloque de filas
  contaba consultas del desvío que no se emiten, y no la lectura de comprobación
  de la muestra; con el bloque de filas contaba los agregados SQL, que el spool
  no corre. En PostgreSQL y SQL Server la estimación de filas del catálogo del
  aviso de costo, y en PostgreSQL `SHOW extra_float_digits`, se mandaban sin
  contarse. `max_consultas` y `meta$consultas$emitidas` cuentan lo mismo que el
  plan, y por bloques lo que el total anuncia tiene su lugar reservado: con
  `max_consultas = attr(plan, "total")` la media de un decimal y
  `perfil_muestra` ya no quedan afuera; con `total_maximo` no queda afuera nada.
  Con un `max_consultas` más chico que el plan pueden entrar menos métricas que
  antes: esas consultas ya llegaban al motor, sin contarse.
- `planificar_limpieza()` sobre un perfil guardado por una versión anterior, con
  un hallazgo por par de columnas iguales, ya no compara cada hallazgo con todas
  las columnas: con 1.000 columnas iguales (499.500 hallazgos), de 87 s a 4 s,
  con el mismo plan.
- El desvío de un decimal que la muestra lee como doble (5e14 con centavos) ya
  no se publica en `perfil_muestra`, en ninguna vía, y la corroboración no lo
  cuenta como divergencia: queda en `perfil_muestra$meta$desvio_decimal_doble`
  y en las no comparadas con la causa `decimal_doble`. Pasaba por la vía por
  omisión en los seis motores con decimales, y `perfilar_coleccion()` subía la
  tabla a `alcance = "divergencias"`. Un desvío que el motor calcula mal sigue
  siendo divergencia.
- Con `universo = "muestra_motor"`, una muestra pedida mayor que la tabla ya no
  se juzga corta: lo efectivo es lo pedido acotado por el total que trae la
  primera consulta. Con `bloque_muestra = "solo_agregados"` se reintentaba y
  se avisaba una degradación falsa en SQLite, MySQL, MariaDB y PostgreSQL 9.3;
  en DuckDB el reintento escribía `TABLESAMPLE BERNOULLI (100)`, que el motor
  rechaza (ahí un número sin unidad es una cantidad de filas), y todas las
  métricas de la tabla quedaban `no_disponible`. `BERNOULLI` se escribe en
  DuckDB con `PERCENT`.
- Con `incluir_valores = FALSE`, el mensaje de un error del motor (una
  conversión fallida, un desborde) ya no cita la celda en `motivo`,
  `meta$consultas$fallas`, la muestra ni los bloques: pasa por el mismo piso
  que con la protección pedida. PostgreSQL, SQL Server y DuckDB lo publicaban
  entero.
- La media de `perfilar()`, de `perfil_muestra`, de `bloque_filas` y de la
  muestra del motor es el doble más cercano de la media exacta. Se acumulaba
  en doble, centrada en la media del primer bloque, y se equivocaba sin aviso
  cuando los valores se cancelan: `c(-2^51, 1, 2, 2^51)` daba 0,6875 por 0,75;
  quince mil valores de 2^51, quince mil de -2^51 y `1:3`, una media de signo
  cambiado; y en `perfil_muestra` la corroboración lo contaba como divergencia
  contra el `AVG` del motor. También sin cancelación: `rnorm(2e6)` quedaba a 19
  ulps, y cuarenta enteros chicos por `bloque_filas = 7`, a ocho. Cada bloque se
  suma ahora sin redondeo, sin depender del `long double` de `sum()`, con el
  mismo costo.
- El desvío de `perfilar()`, de `perfil_muestra`, de `bloque_filas` y de la
  muestra del motor es el doble más cercano del desvío exacto (con `n - 1`). Se
  acumulaba con Welford en doble y quedaba a uno, dos o tres ulps:
  `((1:25) %% 7 - 3) * 0.3` a uno en memoria (como `sd()`), y por
  `bloque_filas = 7` a dos. Ahora la suma de los cuadrados es exacta o tiene una
  cota demostrada, y el redondeo se decide comparando contra los puntos medios
  con aritmética exacta, sin depender del `long double` de `sum()`; el resumen
  de una columna común de 2 millones de filas no tarda más. La media también es
  la más cercana en una columna con valores de más de 2^900 junto a otros
  diminutos (con 2^1000, -2^1000 y valores de 2^-1000 salía 0). Y la rama de
  texto de un entero que pasa de 2^53 saca los centinelas declarados como
  `integer64` —no entraban en los extremos exactos, los conteos ni la mediana—
  y lee `"-0"` como el cero. Un centinela así escrito se reconoce por sus
  cifras también donde R lee ese texto a un ulp del valor (macOS sobre arm64,
  cuyo `long double` es un doble): `"1152921504606846976"` con `2^60`
  declarado quedaba en el resumen y la media salía 2,9e17.
- La mediana de un decimal en SQL Server (compatibilidad 110 o más),
  PostgreSQL 9.4 o más y DuckDB es el doble más cercano de la exacta, por la
  tabla completa y por la muestra del motor: el motor suma sus una o dos
  centrales, como ya en MySQL, MariaDB y SQL Server con compatibilidad 100, en
  lugar de `PERCENTILE_CONT`, que interpola en doble y la dejaba a un ulp. En
  DuckDB la suma se lee por su texto (el controlador convierte un decimal de
  más de 18 cifras a un doble que no es el más cercano), y en SQL Server el
  `money` se suma como `decimal(38,4)`, que no desborda. Cuesta una consulta por
  columna decimal, que `plan_perfilado_dbi()` cuenta.
- En SQL Server, la muestra del motor lee el texto de un decimal de más de 30
  cifras (`CAST(... AS VARCHAR)` es `VARCHAR(30)`, y la lectura de toda la
  muestra abortaba) y `money` con sus cuatro decimales: 0,0001 se leía 0, y el
  mínimo, la media y el desvío de una columna de 0,0001 a 0,0004 salían 0.
- En DuckDB, cuando la media de un `HUGEINT` o `UHUGEINT` cerca de su tope
  desborda, la consulta de mínimo, máximo y conteos se repite sin el promedio:
  esas métricas se juzgan como en cualquier columna que pasa de 2^53, y la media
  queda `no_disponible` con su propio motivo. Antes caían todas con el error del
  motor por motivo, que citaba el valor de la celda aun con
  `incluir_valores = FALSE`.
- En SQLite, el desvío de la muestra del motor ya no pierde cifras con valores
  grandes (`{2^53 - 2, 2^53 - 1}` daba 1 por 0,7071).
- `requisitos_motor()` declara PostgreSQL «9.3 y 16», las dos versiones que se
  miden.
- Un centinela declarado en `sentinelas_numericos` cuenta como ausencia
  también en las dependencias funcionales de `perfilar()` y en las asociaciones
  de `analizar()`: la fila que lo tiene no se evalúa en el par, como con un `NA`,
  y `n_evaluados` y `n_pares` lo dicen. Con un `-999` declarado en una columna de
  palabras, el perfil perdía `x -> y`, porque el centinela era un tercer valor;
  con un `-999` en una columna numérica, la media salía sin él y `analizar()` no
  publicaba una asociación de 0,92. `detectar_dependencias()` y
  `detectar_asociaciones()` reciben el mismo argumento. La lista por omisión
  sigue sin sacar nada de ningún cálculo.
- En una columna de texto, un centinela declarado se reconoce por el número que
  escribe la celda: `"-999"`, `"-999.0"`, `"-9.99e2"` o, con coma decimal,
  `"-999,0"` son el mismo `-999`. El resumen, el detector de faltantes
  disfrazados y la conversión de `planificar_limpieza()` usan esa misma regla.
  Antes el resumen reconocía por sus cifras sólo la forma entera, y el detector
  y el plan comparaban el doble leído: en macOS sobre arm64, donde R lee
  `"1152921504606846976"` como 2^60 − 128, `2^60` declarado y escrito con `.0`
  quedaba en la media, el perfil no lo contaba como disfrazado y `aplicar()`
  registraba la conversión `fallida`. Un texto con más cifras de las que
  distingue un doble es el centinela sólo si lo escribe exacto: en Linux,
  `"1152921504606846976.4"` caía en el doble de `2^60` y se sacaba como si lo
  fuera.
- `perfilar_dbi()` y `plan_perfilado_dbi()` leen las señas de la conexión una
  vez por corrida. Las consultaban al armar cada sentencia, 184 veces sobre una
  tabla de tres columnas, y en duckdb 1.1 cada lectura manda `pragma_version()`
  al motor: 176 sentencias que el plan no anunciaba y `max_consultas` no
  contaba. La ayuda declara fuera de la cuenta las que un controlador manda para
  contestar `DBI::dbGetInfo()` o `DBI::dbIsValid()`, una o dos por corrida.
- `cobertura_analisis()` y `marco_agesic()` recomiendan para cada factor de
  AGESIC la vía que el paquete implementa. Para «No-contradicción» decían
  «Requiere un backend o referencial especializado que no integra esta
  versión.», aunque `EntidadContradictoria` está implementada. La indicación
  (`como_resolverlo`) sale ahora de las métricas que `catalogo_agesic()` marca
  implementadas o por agregación: una receta por métrica, con la cadena entera
  (`especializar()`, `instanciar()`, `medir()`), el contrato en la llamada que
  lo recibe y cada propiedad con el nombre que el código acepta. Varias venían
  mal desde 0.1.0: mandaban a instanciar una métrica sin especializarla, a
  escribir `método`, «expresión», `Comprension` o `dominio`, a declarar
  `vigencia()` en las métricas de oportunidad por atributo, que no la aceptan,
  o a crear `referencial(completo = TRUE)` sin `alcance`, que aborta. El texto y
  la prueba que recorre cada receta hasta una medición salen de los mismos
  pasos.
- Un carácter que la reparación de la codificación reemplaza —el segundo byte de
  una letra mal codificada— ya no cuenta como invisible, y ninguna otra acción
  del plan lo toca. El texto UTF-8 leído como latin-1 deja, en el segundo byte
  de muchas letras, un control C1 («Á» queda `Ã` más U+0081), un guion blando
  («í», U+00AD) o un espacio duro («à», U+00A0); y un C1 suelto puede ser un
  carácter de Windows-1252 (U+0080 por `€`). `perfilar()` publicaba
  `controles_invisibles` además de `codificacion_rota` por un solo defecto. Si
  en el plan se desmarcaba `reparar_codificacion`,
  `eliminar_controles_invisibles` y `normalizar_espacios_invisibles` borraban
  ese byte y dejaban «MarÃa» o «ÃREA», ya sin arreglo y sin hallazgo. Con las
  dos acciones marcadas, `aplicar()` registraba `fallida` la eliminación, que no
  tenía nada que hacer. Ahora las posiciones protegidas salen de la misma
  reparación, y `recortar_espacios`, `reemplazar_separadores` y
  `decodificar_entidades_html` también las respetan. Si el motor no puede
  reparar una celda, sus letras rotas tampoco las toca ninguna acción, y la
  celda sigue señalada. Quedan dos casos sin cubrir. Una comilla tipográfica, un
  guion o un emoji rotos que tienen pegado delante un símbolo que impide la
  reparación (un espacio duro, un `°`) todavía pierden su último byte con
  `eliminar_controles_invisibles`; la celda sigue señalada. Y una letra rota que
  el motor no reconoce como tal —«Š» leída como `Å` más un espacio duro— no
  tiene protección ni hallazgo, como antes. Un control que la reparación deja
  como está, como un U+0081 suelto o un guion blando junto a una letra rota, se
  sigue detectando y quitando. El perfil cuenta un invisible sólo donde la
  acción puede quitarlo, así que el plan ya no ofrece por esa causa una acción
  que después termina `fallida`.
- `aplicar()` corre `reparar_codificacion` antes que las demás acciones de su
  columna, aunque el `orden` editado la ponga después. Con el orden por omisión
  ya era así. Si se la reordenaba detrás de `convertir_mayusculas`, la
  conversión operaba sobre el mojibake: «María» mal leída quedaba «MARíA»,
  con la letra reparada en minúscula.
- Las acciones de texto de `aplicar()` y los detectores de `perfilar()` leen
  bien las celdas declaradas `latin1` que tienen un byte de 0x80 a 0x9F. R
  traduce `latin1` como Windows-1252, que no define 0x81, 0x8D, 0x8F, 0x90 ni
  0x9D: `recortar_espacios` fallaba con «input string is invalid UTF-8» y la
  columna quedaba sin recortar, y otros detectores avisaban o leían esos bytes
  como el texto `<81>`. Es lo que deja un CSV de Windows-1252 leído con
  `encoding = "latin1"`, o uno en UTF-8 leído así: «ÁREA» (`C3 81`) ahora se
  repara. Una celda que una acción cambia queda en UTF-8, como documenta
  `aplicar()`.
  Y la marca no decide nada: una columna declarada `latin1` y la misma columna
  en UTF-8 dan las mismas cuentas en el perfil, las mismas acciones en el plan y
  los mismos valores al aplicarlo, con la reparación marcada o desmarcada. Sólo
  cambia `n_codificacion_normalizada`, que cuenta las celdas `latin1` que una
  acción dejó en UTF-8.
- `planificar_limpieza()` arma `convertir_sentinelas_numericos` con los
  centinelas que el perfil contó en esa columna, y no con el catálogo entero.
  `perfilar()` decide por columna cuáles cuentan —un `999` dentro de una
  numeración de 1 a 2000 no es un centinela; diez `-9`, sí—, pero la acción
  llevaba la lista completa: al marcarla, `aplicar()` convertía también el
  `999` legítimo, y el registro decía 11 celdas donde el plan anunciaba 10.
  Ahora la lista se lee de la evidencia del hallazgo, más lo que se declaró en
  `sentinelas_numericos`. Si la evidencia no alcanza a nombrar todo lo contado
  —una columna de texto con muchas marcas de ausencia distintas—, si una marca
  declarada en `cadenas_ausencia` contiene `; ` o si el perfil se hizo con
  `proteger_datos_personales = TRUE`, la lista sigue siendo el catálogo
  entero, como antes.

# lupa 0.1.0

Primera versión publicada.

## Perfilado

- `perfilar()` describe cada columna de un `data.frame`: el tipo implícito, los
  faltantes reales y los disfrazados (códigos y centinelas), los patrones de
  formato, los formatos de fecha mixtos, los valores extremos, las filas
  duplicadas, las dependencias funcionales y las claves. Lo que no puede medir lo
  declara, con su motivo, en vez de dejarlo en cero.
- `perfilar_dbi()` hace el perfil dentro del motor, por DBI, con una consulta por
  lote y no una por columna, muestras del motor y presupuestos de trabajo
  declarados. Está probado contra PostgreSQL (16 y 9.3), SQL Server 2022, MySQL
  8.4, MariaDB 11.8, SQLite y DuckDB. `perfilar_coleccion()` y
  `relaciones_coleccion()` perfilan varias tablas y proponen las relaciones entre
  ellas, y `estimar_costo()` anticipa lo que va a costar una corrida.
- `analizar()` hace el recorrido completo: el perfil, las distribuciones, las
  asociaciones, el análisis temporal, la clasificación de las variables y una
  propuesta de modelo de medición.
- Cada diagnóstico también se puede pedir por separado:
  `detectar_duplicados_aproximados()`, `detectar_claves()`,
  `detectar_relaciones()`, `detectar_dependencias()`,
  `detectar_formatos_fecha()`, `descubrir_patrones()`, `inferir_tipo()` y
  `distribucion_valores()`.

## Medición de la calidad

- Marcos de calidad con dimensiones y factores que declara el usuario
  (`marco_calidad()`), y los de ISO/IEC 25012, CEPAL y AGESIC listos para usar.
- Métricas reutilizables con su granularidad explícita (`metrica()`,
  `metricas_nucleo()`), modelos de medición (`modelo()`, `especializar()`,
  `instanciar()`), mediciones (`medir()`), agregación tipada (`agregar()`) y
  perfiles de evaluación (`perfil_evaluacion()`, `evaluar()`).
- Validadores de formato: la cédula y el RUT de Uruguay, el correo, la URL, ISO
  3166, ISO 4217, Luhn y mod 97.
- Históricos versionados (`acumular_historico()`, `guardar_historico()`), la
  comparación de dos perfiles (`comparar_perfiles()`) y la detección de deriva
  (`detectar_deriva_calidad()`).

## Limpieza

- `planificar_limpieza()` propone un plan editable de acciones, cada una con su
  justificación y la severidad de lo que la motivó. `aplicar()` lo ejecuta sobre
  una copia y deja el registro de lo que cambió, y `guiar_limpieza()` muestra
  ejemplos para decidir. `lupa` nunca modifica los datos que recibe.

## Informes

- `reportar()` escribe un informe HTML autocontenido, sin depender de software
  externo para generarlo.

## Documentación

- El paquete trae las guías 1 a 6, el camino básico: de la primera tabla al plan
  de limpieza. Las guías 7 a 12 (métricas, marcos, histórico, bases de datos,
  colecciones y escala) y los 4 artículos de referencia están en el sitio,
  <https://sebollin.github.io/lupa/>.

## Rendimiento

- La tabla de formatos de fecha se arma una vez, al instalar el paquete, y no en
  cada columna; el enmascarado de datos personales pasa a pedirse (ver abajo); y
  en los caminos que más se repiten se evitan las llamadas a expresiones regulares
  PCRE, que en Windows cuestan unos 47 µs cada una aunque el vector sea corto.
  Sobre la tabla de ejemplo de 13 filas, `perfilar()` bajó de 0,63 s a 0,16 s
  (0,26 s con la protección pedida) y `analizar()` de 1,36 s a 0,33 s.

## Requisitos

- R 4.2.0 o posterior. Las columnas `integer64` necesitan además bit64 4.6.0 o
  posterior: con versiones anteriores, `match()` de R iguala entre sí todos los
  valores negativos, y `lupa` se niega con la condición `lupa_error_integer64`
  en lugar de publicar distintos, modas y medianas equivocados.

## Datos personales

- La clasificación de posibles datos personales (documentos, nombres, fechas de
  nacimiento) corre siempre, como diagnóstico. Su reemplazo en lo que el paquete
  publica es un complemento que se pide con `proteger_datos_personales = TRUE`:
  publicar o no los valores de una base es decisión de quien la usa.
