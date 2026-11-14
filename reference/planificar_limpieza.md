# Construir y aplicar un plan de limpieza auditable

`planificar_limpieza()` transforma los hallazgos de un objeto `perfil`
en un objeto de datos editable, sin modificar los datos examinados. Cada
fila representa una acción propuesta. Sólo se marcan como recomendadas
las estrategias correctas con independencia del dominio; algunas, como
marcar valores extremos, permanecen inactivas hasta que se decida
actuar. Las decisiones contextuales quedan desactivadas y los formatos
de fecha ambiguos quedan bloqueados.

## Usage

``` r
planificar_limpieza(perfil, datos = NULL, soporte_minimo_dependencia = 2L)

aplicar(plan, datos, permitir_eliminacion = FALSE, conservar_eliminados = TRUE)
```

## Arguments

- perfil:

  Objeto de clase `perfil` creado por
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

- datos:

  `data.frame`, `tibble` o `data.table` sobre el que se ejecuta el plan.
  El objeto recibido no se modifica.

- soporte_minimo_dependencia:

  Cantidad mínima de observaciones concordantes por valor determinante
  para proponer una imputación.

- plan:

  Objeto de clase `plan_limpieza` o data frame con el mismo contrato.
  Puede filtrarse y editarse antes de aplicarlo; una acción cuyos
  parámetros no son la lista con nombres que propone el plan —un vector,
  `NULL`— queda `fallida` con ese motivo.

- permitir_eliminacion:

  Segundo consentimiento obligatorio para ejecutar acciones que eliminan
  filas o columnas. Sin él, una acción eliminatoria seleccionada hace
  que `aplicar()` **se niegue antes de tocar nada**: no se ejecuta
  ninguna acción del plan —tampoco las que no eliminan— y no hay
  registro, porque no hubo ejecución. El error nombra las acciones y los
  dos caminos: dar el consentimiento, o desactivarlas y aplicar el
  resto.

- conservar_eliminados:

  Si se conservan en el resultado las filas y columnas retiradas. Es
  `TRUE` de forma predeterminada.

## Value

`planificar_limpieza()` devuelve un data frame de clase `plan_limpieza`.
`aplicar()` devuelve una lista de clase `resultado_limpieza` con
`datos`, `registro`, `plan_aplicado`, el `plan` sincronizado y
`eliminados`. El `registro` conserva `estado` (`ejecutada` o `fallida`),
`error`, `n_no_reversibles` y la `justificacion` de cada acción
seleccionada, incluso cuando una falla y las siguientes continúan.
`n_codificacion_normalizada` cuenta, por acción, las celdas cuya **marca
de codificación** cambió: es el caso de una acción de texto sobre celdas
`latin1`, cuyo valor se conserva y cuyos bytes no. No cuenta una celda
sin marca cuyos bytes ya eran UTF-8 válido ni una celda que es o queda
ASCII, así que no depende del locale. `n_no_reversibles` cuenta las
celdas cuyo VALOR se perdió y no se puede recuperar desde el resultado:
un centinela `-999` que pasa a ausencia, un extremo recortado a su
límite, un marcador de ausencia convertido, o un número que se redondeó
al convertirlo. Las acciones que **normalizan la escritura** —recortar
espacios, quitar un invisible de transporte, reemplazar separadores o
cambiar mayúsculas— dejan el valor en su lugar y no cuentan, **salvo
cuando la normalización fusiona valores que eran distintos**: ahí lo que
los separaba no queda en ningún lado y esas celdas sí se cuentan. El
criterio se mide sobre el resultado, no por el nombre de la acción:
quitar un guion suave de `PRO<U+00AD>DUCTO-A` no pierde nada, y quitar
un espacio de ancho cero que distinguía dos claves fusiona las dos. Si
una acción seleccionada no produce ningún efecto, se registra como
`fallida` con el motivo y su copia no se incorpora al resultado. La
comprobación del efecto observa `n_cambiadas` aunque `n_afectadas` sea
`NA` o haya sido editado: una acción seleccionada que no cambia nada
queda `fallida`, porque la ausencia de efecto es observable sin depender
de la estimación. Si una columna de entrada es un factor, las acciones
que transforman su texto devuelven una columna `character`: no se
reconstruyen los niveles originales, porque una limpieza puede
introducir valores nuevos. Los atributos de cada columna —la etiqueta de
variable que deja `haven`— sobreviven a las acciones que la reescriben,
a las conversiones de tipo y a las que quitan filas; quitar filas de un
`data.table` conserva su clave.

## Details

`aplicar()` ejecuta exclusivamente las filas con `aplicar == TRUE`,
sobre una copia de `datos`. Verifica que cada columna siga siendo
identificable y que las conversiones sean completas antes de
sustituirla. Devuelve los datos nuevos junto con un registro de las
acciones y sus parámetros. El mismo registro queda en el atributo
`registro_limpieza` de los datos resultantes.

Las alternativas para un mismo hallazgo comparten `grupo`; las acciones
independientes usan `NA`. Como máximo una alternativa de cada grupo
puede tener `aplicar == TRUE`, invariante que `aplicar()` vuelve a
validar. No se agrega una fila ficticia para "no hacer nada":
`decision_grupo` distingue `pendiente`, `recomendada`, `desactivada`,
`elegida` y `omitida`, mientras `recomendacion_grupo = "no_hacer_nada"`
representa una recomendación explícita de conservar los datos. Esto
permite separar un grupo aún no revisado de una omisión deliberada.

`estado` distingue acciones `lista`, `bloqueada` e `informativa`;
`orden` fija la secuencia reproducible. La acción
`reparar_codificacion`, cuando está seleccionada, tiene precedencia
sobre las demás acciones de su misma columna: corre primero en esa
columna para que las restantes reciban texto ya decodificado, aunque su
`orden` editado sea mayor. No reordena acciones de otras columnas. Del
mismo modo, `decodificar_entidades_html` corre antes que
`convertir_minusculas`, `convertir_titulo` y `convertir_mayusculas` de
su columna aunque un `orden` editado la ponga detrás: una entidad con
nombre distingue mayúsculas —`&eacute;` es «é» y `&Eacute;` es «É»—, y
cambiar la caja antes de decodificar cambiaba la letra o dejaba la
entidad sin leer (`&Amp;`). Con el `orden` del plan ya corre antes y
nada se mueve. Entre las acciones restantes, y entre empates, el
desempate lo resuelve `id_accion`, no la posición de la fila: reordenar
el plan no cambia el resultado. Y un grupo marcado como `elegida` sin
ninguna acción activa se rechaza, porque no es ni una elección ni una
omisión. `n_afectadas` es la estimación del perfil sobre **lo que esta
acción tocaría**, que puede ser menos que el conteo del hallazgo que la
originó cuando la acción sólo cubre parte del caso: una columna con tres
valores de codificación rota, de los cuales uno es reparable, produce un
hallazgo con `n_afectados = 3` y una acción `reparar_codificacion` con
`n_afectadas = 1`. Las dos cifras son ciertas y cuentan cosas distintas.
`unidad_conteo` dice si cuenta filas, columnas o valores distintos —lo
declara la acción cuando cuenta en una unidad propia, y sólo si no lo
hace se hereda del hallazgo—. El registro informa `n_cambiadas` sobre
los datos recibidos.

Con un universo declarado, `n_afectadas` también cuenta lo que la acción
va a cambiar. Las acciones por celda no tocan las filas que
`aplicabilidad` deja fuera, y tampoco las cuentan. Una conversión de
tipo cambia la columna entera —una columna tiene un solo tipo—, así que
cuenta todos sus valores presentes: los del universo, los de fuera y los
de las filas donde la regla no se pudo decidir. Las celdas vacías no se
cuentan: fuera del universo están vacías por diseño, y en una columna de
`columnas_opcionales` el universo son justamente las celdas con valor.
Aplicada sola sobre los datos del perfil, la conversión registra en
`n_cambiadas` lo que anunció.

**`n_afectadas` y `n_cambiadas` pueden no coincidir, y las dos son
ciertas.** La estimación se calcula sobre los datos que se perfilaron;
el registro cuenta lo que pasó al aplicar. Si una acción anterior del
mismo plan ya tocó esa columna, la posterior encuentra menos —o más— de
lo estimado. Las excepciones de precedencia son dos:
`reparar_codificacion`, que se adelanta a las demás acciones de su
columna para que ninguna opere sobre mojibake, y
`decodificar_entidades_html`, que se adelanta a las conversiones de
caja. Por ejemplo, con `convertir_sentinelas_numericos` (orden 110)
convirtiendo tres `-999` en ausentes, `winsorizar_outliers` (orden 520)
recorta dos valores donde el plan estimaba siete. No es un desvío que
ocultar: `orden`, `n_afectadas` y `n_cambiadas` se publican los tres, y
compararlos es la forma de ver el efecto de la composición. Sólo el caso
extremo —la acción no produce **ningún** efecto, también cuando el plan
no trae una estimación válida— se registra como `fallida` con su motivo,
salvo que una acción anterior del mismo plan ya haya hecho su trabajo:
si la misma detección con que el plan contó la acción encontraba el
problema en los datos que recibió `aplicar()` y ya no lo encuentra
cuando a la acción le toca correr, la acción queda `ejecutada` con
`n_cambiadas = 0` y sin `error`, y el resumen impreso no la cuenta entre
las fallidas. Por ejemplo, `convertir_ausencias_textuales` (orden 100)
vuelve ausentes las celdas en blanco y a `recortar_espacios` (orden 200)
no le queda nada que recortar en ellas. Esa detección existe para las
acciones de texto que el perfil cuenta con `n_espacios_borde`,
`n_invisibles_eliminables`, `n_espacios_invisibles`, `n_entidades_html`,
`n_separadores_en_campo` y `n_codificacion_reparable`:
`recortar_espacios`, `eliminar_controles_invisibles`,
`normalizar_espacios_invisibles`, `decodificar_entidades_html`,
`reemplazar_separadores` y `reparar_codificacion`. Si el problema sigue
en los datos y la acción no lo tocó, si nunca estuvo en los datos que
recibió `aplicar()` —un plan aplicado a otra entrega— o si la acción no
tiene esa detección, sigue `fallida`.

**Esa comparación sólo lee composición cuando las dos cifras cuentan en
la misma unidad.** `n_afectadas` cuenta en la `unidad_conteo` que el
plan declara y `n_cambiadas` cuenta lo que la acción tocó al aplicarse,
que es su unidad natural. Cuando la acción declara
`unidad_conteo = "valor_distinto"` —las conversiones de mayúsculas y
minúsculas—, las dos cifras miden poblaciones distintas y su diferencia
no dice nada sobre la composición: medido sobre
`c("Ana", "ana", "ANA", "Beto", " Ana ")`, `convertir_minusculas` estima
`n_afectadas = 3` valores distintos y el registro publica
`n_cambiadas = 4` celdas. Las dos son ciertas. La unidad del plan se
recupera uniendo el registro con el plan por `id_accion`, que los dos
publican. `reversible` indica si la conversión conserva la identidad de
cada valor. Las conversiones se comprueban sobre todos los valores de
`datos`: las numéricas bloquean ceros iniciales, colisiones no
inyectivas y valores con más cifras significativas de las que guarda un
número de doble precisión —un decimal se compara con la precisión con
que vino escrito—, mientras que fechas, fechas-hora y lógicos sólo
bloquean conversiones no ejecutables o no inyectivas. Las fechas pueden
cambiar a la representación canónica del tipo sin que eso sea una
pérdida. Sin `datos` no se puede hacer la comprobación y la acción queda
bloqueada. Cuando **una conversión de tipo** se ejecuta y no es
reversible se marca `destructiva` —también si la columna no era segura y
se activa a mano—, no se activa por defecto y el registro conserva
`n_no_reversibles` y la justificación de la decisión.

`destructiva` no es sinónimo de "pierde algo": marca las acciones que el
usuario tiene que activar a mano —las que retiran filas o columnas y las
conversiones que pierden representación, como `winsorizar_outliers`
sobre una columna **guardada como entera**, donde los límites de Tukey
son cuartiles y la columna queda en doble precisión: la justificación lo
dice y `parametros` publica `tipo_original` y `tipo_resultante`. Sobre
cualquier otra clase —texto con números, factor, fecha— la acción
conserva la clase y reescribe sólo las celdas recortadas; `Inf`, `NaN` y
`NA` quedan como estaban— y por eso ninguna acción `destructiva` puede
estar `recomendada`. Hay acciones recomendadas que sí pierden el valor
de una celda: `convertir_ausencias_textuales` cambia un marcador por
`NA` y `eliminar_controles_invisibles` quita un carácter. Esas lo dicen
en su justificación y el registro las cuantifica en `n_no_reversibles`;
leer `destructiva = FALSE` no significa que no se haya perdido nada,
sino que el paquete pudo recomendar la acción sin conocer el dominio.
Sobre una columna **factor** las acciones por celda devuelven texto: el
resultado no puede ser un factor incompleto, así que el orden declarado
y los niveles sin observaciones no se conservan. La justificación de la
acción lo dice, y si el factor es ordenado la acción queda recomendada
pero **sin activar**, porque conservar el orden es una decisión del
dominio.

Tres atributos del plan declaran lo que el plan no cubre.
`cobertura_diagnosticos` trae los diagnósticos que el perfil no pudo
evaluar, para que leer tres acciones no se confunda con "lo demás está
bien". `hallazgos_sin_accion_por_columna_ambigua` trae los hallazgos
medidos que no produjeron acción porque su columna no se puede
identificar: comparte nombre con otra y no hay forma de saber sobre cuál
actuaría la limpieza —el remedio es normalizar los nombres y volver a
perfilar—, o no está entre las columnas del perfil, o no tiene nombre
—su nombre es `NA`; una columna llamada con el texto `"NA"` sí lo tiene,
y el plan actúa sobre ella—; el `motivo` dice cuál. Un hallazgo de
varias columnas —una relación aritmética en `neto,iva,total`, una de
orden— no es una columna ambigua: lo es sólo si alguna de sus columnas
lo es. Sus columnas se leen de la trazabilidad del hallazgo y no del
texto de `columna`: una columna llamada `a,b` no se confunde con la
relación entre `a` y `b`. `hallazgos_sin_accion` trae el resto: los
hallazgos que el perfil midió, de una columna identificada, de varias o
de la tabla, y que ninguna acción del plan atiende —porque el plan no
tiene una estrategia para ese tipo, o porque la que tiene necesitaba una
condición que no se cumplió—. Su `motivo` dice si la misma columna (o
esas columnas) recibió otras acciones, sin afirmar que alguna cubra el
hallazgo: eso es una decisión del dominio. Un hallazgo `ok` no espera
acción y no entra a ninguno de los dos. Los tres se imprimen con el
plan.

`convertir_numero_regional` sólo se recomienda si todos los valores
presentes comparten convención decimal, unidad y moneda. En una columna
que el perfil infiere como `identificador` (`101a`, `102a`) la acción
queda `lista` sin activar: lo que acompaña al número puede ser parte del
código. Un valor con `%` se divide por 100, la escala con que el paquete
publica toda proporción; queda en `[0, 1]` si el porcentaje estaba entre
0 y 100, y el plan no comprueba ese rango. Una unidad, o una moneda
delante o detrás del número (`$ 35`, `1.234,56 $`), dejan de formar
parte del valor y quedan en `parametros`. La justificación lo dice
cuando ocurre. Una columna que mezcla `5` con `5 %` no se convierte:
habría que decidir si el número sin `%` está en la misma escala. Esta
conversión no lee la notación científica (`1e5`): si la columna la trae
junto a números con formato regional, la acción queda `bloqueada`.
`convertir_tipo` lee cada texto con la convención decimal de la columna,
como el resumen del perfil: con coma decimal, `12.500` se convierte en
doce mil quinientos. La acción de codificación prueba las tablas
congeladas de varias codificaciones y deja en `estado_reparacion` uno de
`reparado`, `reparado_parcialmente` o `no_se_pudo`: el de lo que la
acción, tal como sale del plan, va a cambiar. Una reparación parcial
—una celda que el motor repara sólo en parte— no se aplica
automáticamente, pero tampoco frena la de las celdas que se reparan
enteras. En una columna sin parciales, la acción sale activa —si nada
más lo impide— y repara todas las celdas que el motor repara. En una
columna con parciales y con celdas que se reparan enteras, la acción
también sale activa, con `aplicar_parciales = FALSE` en `parametros`:
repara las enteras, deja las parciales como están, `n_afectadas` cuenta
sólo las enteras y la justificación dice cuántas parciales quedan; para
aplicarlas también, se pone `aplicar_parciales = TRUE` en `parametros`.
En una columna que sólo tiene parciales, la acción queda `lista` con
`aplicar = FALSE` y `aplicar_parciales = TRUE`, y se selecciona de forma
explícita marcando `aplicar = TRUE`; entonces se aplican las parciales.
Una celda sin reparación local —con el carácter de reemplazo U+FFFD, o
que el motor no pudo reparar— no frena la de las demás: queda como está
y la justificación de la acción la cuenta. La estrategia se llama
`reparar_codificacion` y no limita el motor a latin-1. Un texto legítimo
que se escribe con los mismos bytes que un mojibake —por ejemplo, uno
que cita literalmente la `Ã` seguida de `©`— no se distingue de uno
roto: se acusa, la evidencia del plan lo muestra antes de aplicar, y si
se aplica la acción se reescribe como la letra que esos bytes codifican.
Si la columna contiene texto así, no active la acción. Unos mismos bytes
pueden parecer un control: el segundo byte de una «Á» mal leída es un
C1, el de una «í» un guion blando, y `c2 80` es la huella de un euro de
Windows-1252 leído como latin-1. Lo que la reparación reemplaza no
cuenta como carácter invisible y ninguna otra acción del plan lo toca:
aunque se desmarque `reparar_codificacion`, la secuencia queda entera y
el perfil siguiente la sigue encontrando. Lo mismo si el motor no puede
reparar la celda: sus letras rotas quedan como están. Los controles que
la reparación deja como están, como un U+0081 suelto o un guion blando
fuera de una letra rota, se diagnostican y se quitan, también en una
celda con mojibake, salvo que quitarlos vuelva reparable la celda:
entonces la acción la deja como está, para que el resultado no dependa
de si corre antes o después de `reparar_codificacion`. El perfil cuenta
un invisible entre los que se pueden eliminar
(`n_invisibles_eliminables`) sólo donde la acción puede quitarlo; el
control que la acción deja así sigue contando en
`n_controles_invisibles`, y si es el único el hallazgo dice que ninguna
acción del plan lo quita, en vez de recomendar quitarlo. La reparación
devuelve el texto que codificaban los bytes, y si ese texto trae un
invisible —el mojibake de un U+200B se repara a un U+200B— el plan no lo
quita en la misma pasada: el perfil describe los datos que recibió. El
perfil del resultado lo señala, y su plan lo quita. Una celda cuyo texto
no se puede leer —declarada `bytes` con bytes que no son UTF-8, o sin
marca en una sesión UTF-8, que es lo que deja
[`read.csv()`](https://rdrr.io/r/utils/read.table.html) sin
`fileEncoding` sobre un archivo latin1— no se transforma: el perfil no
la midió y la informa como `codificacion_invalida`, cuyo remedio es
volver a leer la fuente declarando su codificación. Las acciones de
texto trabajan sobre las demás celdas de la columna y cuentan sólo lo
que cambiaron. Una celda sin marca cuyos bytes son UTF-8 válido se trata
como UTF-8 en cualquier locale, igual que en el perfil: con `LC_CTYPE=C`
las acciones hacen lo mismo que en una sesión UTF-8.

La protección de las acciones de texto se decide por una propiedad
común, no por una lista de estrategias: en cada celda que el perfil
señala como rota y reparable se comparan los dos órdenes de reparación y
acción. Si no conmutan, la celda conserva su valor original y no cuenta
entre las cambiadas. La comprobación se ejecuta sólo sobre las celdas
que la acción iba a cambiar. El perfil cuenta con la misma regla:
`n_afectadas` es lo que la acción sola puede cambiar, y el plan no la
ofrece por las celdas que la protección le reserva (el espacio del borde
que deja reconocer una letra rota no cuenta como `espacios_sobrantes`).
La excepción es `entidades_html`: la entidad se informa aunque esté
junto a una letra rota; `decodificar_entidades_html`, que no viene
marcada, la decodifica después de la reparación y, sola, deja esa celda
como está.

Una celda `latin1` **sí** se transforma —ahí R conoce la codificación y
la convierte sin pérdida—, y el resultado queda marcado `UTF-8`. Como la
acción toca sólo las celdas que cambia, una columna `latin1` puede
quedar con marcas **mixtas**: las celdas transformadas en `UTF-8` y las
intactas en `latin1`. El valor es el mismo, pero los bytes no, así que
el registro lo cuenta en `n_codificacion_normalizada` en vez de dejarlo
implícito. Ese conteo es de recodificaciones, no de marcas: una celda
sin marca cuyos bytes son UTF-8 válido ya es UTF-8 y no cambia de
codificación aunque la acción la devuelva marcada, y un texto ASCII, el
de entrada o el que queda, se escribe igual en toda codificación
—decodificar `Jos&eacute;` o recortar un espacio duro del borde cambia
el valor, no la codificación—. No se convierte la columna entera a
propósito: eso tocaría celdas que el plan no declaró como cambiadas y
haría mentir a `n_cambiadas` por el otro lado. Si se marca una acción
que no está `lista`, `aplicar()` aborta antes de modificar la copia y
enumera las filas problemáticas. Una acción que sí está lista pero falla
se registra con su error y no impide aplicar las siguientes: cada una
conserva atomicidad sobre su propia columna o tabla. Una acción de
nombres preparada para otros nombres —un perfil de otra versión de la
tabla, con dos `x` donde los datos tienen una— es una de esas: queda
`fallida` con el motivo, y en un plan armado con `datos` ya sale
`bloqueada`. Las acciones que efectivamente eliminan filas o columnas
requieren además `permitir_eliminacion = TRUE`; una conversión
`destructiva` requiere selección explícita y deja la pérdida
cuantificada. Por defecto, el resultado conserva lo retirado en
`eliminados`; use `conservar_eliminados = FALSE` para evitar ese costo
de memoria.

Los hallazgos `controles_invisibles`, `entidades_html` y
`separadores_en_campo` tienen acciones separadas. La detección de
invisibles informa tanto los caracteres que se pueden normalizar como
los que pueden ser parte del texto —ZWJ/ZWNJ y las etiquetas Unicode que
arman banderas—; la normalización actúa sobre un conjunto más pequeño
que la detección. La acción `eliminar_controles_invisibles` quita
controles C0/C1 que no son separadores y los invisibles Unicode de
transporte —también los operadores invisibles, los rellenos del hangul y
la etiqueta de idioma U+E0001—, y se recomienda por defecto; conserva
ZWJ/ZWNJ y las etiquetas de bandera. `normalizar_espacios_invisibles`
colapsa espacios Unicode (incluido NBSP) a un espacio ASCII y, como su
hermana, se recomienda y se activa por defecto: queda un espacio, así
que el valor sigue ahí; si al colapsarlo dos valores que eran distintos
quedan iguales, el registro lo cuenta en `n_no_reversibles`. Corre antes
que `recortar_espacios`, así que el espacio común que deja en el borde
lo quita el recorte. Para `recortar_espacios` y para `n_espacios_borde`,
un espacio al borde es todo blanco ASCII —espacio, tabulación, salto de
línea, tabulación vertical, avance de página y retorno— y todo separador
de Unicode, como el espacio duro (U+00A0) o el fino (U+202F): los mismos
que recortan los validadores. El espacio duro que es la mitad de una
letra mal convertida no se cuenta ni se recorta.
`decodificar_entidades_html` cubre las entidades con nombre comunes en
español y referencias numéricas válidas, pero no se activa sola porque
un ampersand puede ser contenido legítimo. Decodifica **una capa**: un
texto codificado dos veces —`&amp;amp;`— queda en `&amp;`, y el perfil
del resultado lo vuelve a señalar. No se itera hasta el fondo porque no
se puede distinguir una doble codificación de un texto que quería decir
`&amp;`: la segunda capa se decide viéndola. `reemplazar_separadores`
convierte tabulaciones, saltos de línea, avances de página y
tabulaciones verticales (`\\t`, `\\n`, `\\r`, `\\r\\n`, `\\f` y `\\v`)
en un espacio y también requiere una decisión explícita. Son exactamente
los códigos 9 a 13 que cuenta el hallazgo: los separadores Unicode
U+2028 y U+2029 son de `normalizar_espacios_invisibles` y el control
U+0085 de `eliminar_controles_invisibles`, y sobre celdas `bytes` la
acción no toca ningún byte de un carácter multibyte. Las tres acciones
registran el número de valores cambiados. Una comparación aproximada con
`normalizar = TRUE` usa estas mismas clases: colapsa espacios y omite
basura de transporte, pero conserva ZWJ/ZWNJ y las etiquetas.

Las imputaciones por dependencia funcional se ofrecen desactivadas.
Aunque una dependencia exacta permite deducir un valor sin usar media,
moda o un modelo externo, sigue siendo una regularidad aprendida de una
sola entrega y puede reflejar un error sistemático en vez de una regla
de negocio. El plan conserva el mapa y su soporte para que el usuario la
confirme; sólo entonces se aplica y se vuelve a validar contra los datos
recibidos. Si la protección enmascaró alguna celda del mapa —una clave o
un valor, también cuando ninguna de las dos columnas es personal pero
una celda contiene un valor protegido, como la sucursal que se llama
igual que un titular—, queda marcado `mapa_enmascarado = TRUE` y se
valida igual que sin protección: las filas que el plan publica en claro
son el mapa tal cual, y una fila tapada se resuelve con el par de los
datos recibidos que tiene su forma y que, tapado con los valores
protegidos de esos mismos datos, da lo que el plan publicó; la relación
de los datos se toma con el soporte declarado, sin publicar sus valores.
Si los datos contradicen una fila —la publicada en claro o la tapada—,
la acción queda `fallida`, y una clave que no está en el plan no se
imputa: reconstruir el mapa sobre los datos recibidos escribiría, en
otra entrega, valores que el plan nunca mostró. Una fila tapada no se
puede validar si los datos recibidos no traen el valor protegido que la
tapó —un subconjunto sin ese titular—: también queda `fallida`, con el
motivo, y se aplica planificando sobre esos datos. Y si los datos piden
imputar una clave cuya fila el plan publicó tapada pero no traen ningún
par de esa clave que la resuelva —la entrega que trae sólo las filas a
completar—, la acción tampoco escribe a medias: queda `fallida`, con el
motivo, porque sin la protección el plan escribiría esas celdas y con la
fila tapada no se sabe con qué. Con una regla de `aplicabilidad` sobre
la columna dependiente, la imputación respeta su universo como las demás
acciones por celda: no llena las filas declaradas fuera, que son vacío
por diseño, y `n_afectadas` las excluye.

Cuando el determinante o el dependiente es una columna protegida, se
tapa por construcción todo ese lado del mapa: el texto usa
`[valor protegido]` y las columnas numéricas, `Date` y `POSIXct` usan
`NA`. El lado no protegido conserva el resultado de los barridos
generales, incluido el piso por contenido. Los demás parámetros de una
acción sobre una columna protegida tampoco publican objetos de clase
`Date` o `POSIXct`.

Con un lado entero tapado, todas sus filas quedan tapadas y `aplicar()`
sólo puede resolverlas con datos que traigan la relación y el soporte
declarado. Una entrega parcial —las filas a completar y un par de cada
clave— queda `fallida`, con el motivo de que los datos no traen un par
de esa clave que la resuelva. Una contradicción en una clave que no hay
que completar —una fila con el dependiente presente que cambió de valor—
no se detecta: el plan no publica esa clave para compararla, la acción
sale `ejecutada` y lo que imputa es correcto sobre los datos recibidos.
Es una limitación de esta representación tapada.

Sobre una columna protegida, `convertir_sentinelas_numericos` publica el
catálogo de centinelas del paquete tal cual —es vocabulario, no un dato—
y tapa el resto. Lo tapado se marca con `valores_enmascarados = TRUE` y
`aplicar()` lo resuelve sobre los datos que recibe: son los rellenos de
la columna, el dígito repetido que ocupa el lugar de un documento. El
plan ofrece la acción así sólo si esa resolución convierte, sobre los
datos del plan, exactamente las celdas que estima; si no, la acción
queda `bloqueada` y la justificación dice cómo ejecutarla. Lo mismo en
una columna que no es personal cuando el centinela declarado aparece en
una protegida —el `99999999` que se repite en la cédula de una tabla de
movimientos—: no se publica, se declara y se resuelve con los rellenos
de esa columna, o la acción queda `bloqueada`. Un centinela declarado
que no aparece en ninguna protegida se publica: es la declaración del
usuario.

`marcar_filas_duplicadas` añade dos columnas. `.fila_duplicada`
reproduce la semántica de
[`duplicated()`](https://rdrr.io/r/base/duplicated.html) y marca sólo
las apariciones posteriores; `.grupo_duplicado` identifica a **todas**
las filas que participan en cada grupo de contenido idéntico. Las
acciones de duplicados comparan las filas con la misma igualdad que el
perfil, también con columnas `integer64`, matrices, `data.frame`
anidados o `POSIXlt`; una tabla con columnas de lista deja las acciones
bloqueadas. Las de columnas duplicadas comparan el contenido como el
perfil: un factor con nivel `NA` y el mismo factor sin ese nivel, un
`POSIXlt` y su `POSIXct`, o dos factores sin orden con los mismos
niveles en otro orden, son la misma columna también al aplicar. Si
alguna columna del grupo se llama como otra de la tabla, no se propone
acción: el hallazgo queda en `hallazgos_sin_accion_por_columna_ambigua`,
con el remedio. Hay una decisión por grupo de columnas iguales, no por
par: marcarlo, o eliminar las repetidas y conservar la primera del
grupo. `columna_1` nombra la que se conserva, y `columna_2` y
`eliminar`, todas las repetidas; el atributo
`columnas_duplicadas_marcadas` lleva una fila por repetida, junto a la
primera. Marcar no elimina filas. Con las dos columnas incluidas, las
marcas distinguen la primera aparición de las repetidas, pero no a las
repetidas entre sí: en un grupo de tres o más filas un perfil posterior
sigue contando duplicadas. Para saber si los duplicados siguen en la
tabla hay que quitar las columnas de marca antes de perfilarla.

Al volver a planificar sobre lo aplicado, el plan no propone acciones
sobre las marcas que agrega —`.fila_duplicada`, `.grupo_duplicado`,
`.ausente_<columna>` y `.outlier_<columna>`—: sus hallazgos quedan en
`hallazgos_sin_accion` con ese motivo, porque el `NA` de
`.grupo_duplicado` significa que la fila no participa de ningún grupo y
no una ausencia. Una acción cuya columna de marca ya existe en los datos
queda `bloqueada`: `aplicar()` no puede volver a crearla.

El orden operativo se aparta deliberadamente de la secuencia dimensional
frescura–completitud–exactitud–consistencia–unicidad sugerida por el
marco. Primero marca duplicados sin borrar, luego normaliza ausencias y
texto, y deja los cambios de esquema para el final. Esto evita perder la
evidencia original, permite imputar antes de convertir tipos y mantiene
identificables las columnas durante todo el plan. Las eliminaciones
nunca se activan por defecto, por lo que deduplicar temprano no puede
hacer desaparecer registros. `destructiva` también marca una conversión
que pierde representación, aunque no elimine filas o columnas. El
consentimiento `permitir_eliminacion` sólo se exige para las estrategias
que efectivamente retiran filas o columnas; una conversión destructiva
requiere que el usuario la active explícitamente y deja su pérdida
cuantificada en el registro.

**Marcar o eliminar ausentes va después de normalizarlos**, no antes.
Una columna con `"N/A"` y `"sin dato"` tiene ausentes disfrazados que
`convertir_ausencias_textuales` convierte en `NA` reales; si la marca
corriera primero, quedaría una columna `.ausente_x` que dice `FALSE` en
filas que terminan en `NA` —una afirmación falsa sobre los datos,
publicada con las dos acciones en `ejecutada` y sin error—. El plan
ordena la marca después de la conversión, así que el idioma documentado
`plan$aplicar <- plan$recomendada` no produce esa marca falsa. Por la
misma razón, `eliminar_filas_ausentes` también va después: eliminar
antes deja sin eliminar las filas cuyo ausente todavía estaba
disfrazado.

`convertir_ausencias_textuales` convierte exactamente los textos de
`parametros$valores`, comparados sin mayúsculas ni espacios al borde. El
plan arma la lista con los marcadores que el perfil detectó y los que se
declararon en `cadenas_ausencia`; editarla es una decisión del usuario:
quitar un texto lo conserva y agregar uno lo convierte, como si se
hubiera declarado. Con protección, la lista y la declaración guardan su
valor también sobre una columna protegida: un marcador del catálogo
—`sin dato`— o uno declarado —`SIN REGISTRO`— no es el dato de nadie, y
la declaración ya se publica en `meta$cadenas_ausencia`. Tapados,
saldrían `[valor protegido]` y `aplicar()` registraría `fallida` la
acción recomendada.

`convertir_sentinelas_numericos` convierte los números de
`parametros$valores`. El plan arma la lista con los centinelas que el
perfil contó en esa columna —los que la traza del hallazgo publica en
`sentinelas_contados`— y con los que se declararon en
`sentinelas_numericos`: un `999` dentro de una numeración de 1 a 2000 no
es un centinela para
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md), y
la acción no lo convierte. Así, aplicada a los datos del perfil, cambia
exactamente las celdas que `n_afectadas` anuncia. Una celda que el
perfil contó como marca textual declarada en `cadenas_ausencia` —un
`"999"` con `cadenas_ausencia = "999"`— es de
`convertir_ausencias_textuales`, y esta acción no la toca aunque se lea
como uno de sus números. La acción trabaja sobre números, texto,
factores, duraciones (`difftime`) y medidas con unidad (`units`), y
conserva la clase y la unidad de la columna. Editar la lista es una
decisión del usuario, como en la acción de las ausencias textuales.

Sobre otros datos —otra entrega de la misma tabla— la lista manda: la
acción convierte esos valores y ningún otro. Si la otra entrega usa otro
código, la acción termina `fallida` sin cambiar celdas, y hay que volver
a perfilar o agregar el código a `valores`.

En dos casos el plan no sabe qué centinelas contó el perfil: un perfil
guardado por una versión anterior, cuya traza no trae el campo, y un
hallazgo cuya evidencia tapó la protección de datos personales, porque
el perfil no publica ahí qué valor contó. Entonces `valores` es la lista
entera de centinelas del perfil, la justificación lo dice, y la acción
lleva en `parametros$guarda` el contexto con que el perfil decidió —el
tipo inferido, la convención decimal y las banderas de la numeración—.
Al aplicar, convierte sólo los centinelas de esa lista que el mismo
detector cuenta en los datos que recibe, que pueden ser menos, y nunca
un valor que no esté en `valores`; lo que el usuario agregue a `valores`
se convierte sin acotar. Sobre los datos del perfil convierte lo
contado. Sobre otra entrega decide con esos datos, y puede convertir un
centinela de la lista que el perfil de origen no había contado: es el
límite de estos dos casos. Si el perfil contó dentro de un universo
declarado en `aplicabilidad`, la acción necesita la regla que el plan le
copia para acotar: sin ella termina `fallida` y dice por qué.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md),
[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md)

## Examples

``` r
datos <- data.frame(categoria = c(" A", "S/D", "B"))
perfil <- perfilar(datos)
plan <- planificar_limpieza(perfil, datos)
plan[, c("grupo", "estrategia", "recomendada", "aplicar")]
#>   grupo                    estrategia recomendada aplicar
#> 1  <NA> convertir_ausencias_textuales        TRUE    TRUE
#> 2  <NA>             recortar_espacios        TRUE    TRUE
resultado <- aplicar(plan, datos)
resultado$datos
#>   categoria
#> 1         A
#> 2      <NA>
#> 3         B
```
